#!/usr/bin/env python3
"""Event-driven battery notifications via UPower DBus.

Run tests: ./battery-notify.py --test
Type check: pipx run mypy battery-notify.py
Format: pipx run ruff format battery-notify.py
"""

import subprocess
from enum import IntEnum
from typing import TypeVar

T = TypeVar("T", int, float)

# Must be sorted low to high
THRESHOLDS = [3.0, 5.0, 10.0]
DEVICE = "/org/freedesktop/UPower/devices/DisplayDevice"


class BatteryState(IntEnum):
    UNKNOWN = 0
    CHARGING = 1
    DISCHARGING = 2
    EMPTY = 3
    FULLY_CHARGED = 4
    PENDING_CHARGE = 5
    PENDING_DISCHARGE = 6


GVARIANT_TYPES: dict[str, type] = {
    "double": float,
    "uint32": int,
    "int32": int,
    "uint64": int,
    "int64": int,
    "string": str,
}


def get_dbus_property(prop: str, typ: type[T]) -> T:
    """Get a UPower device property via D-Bus, parsed as typ."""
    output = subprocess.run(
        [
            "gdbus",
            "call",
            "--system",
            "--dest",
            "org.freedesktop.UPower",
            "--object-path",
            DEVICE,
            "--method",
            "org.freedesktop.DBus.Properties.Get",
            "org.freedesktop.UPower.Device",
            prop,
        ],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    # Parse GVariant like '(<double 42.0>,)' or '(<uint32 2>,)'
    inner = output.strip().removeprefix("(<").removesuffix(",)").removesuffix(">")
    parts = inner.rsplit(" ", 1)
    if len(parts) == 2:
        gvariant_type, value = parts
        expected = GVARIANT_TYPES.get(gvariant_type)
        if expected is None:
            raise ValueError(f"Unknown GVariant type: {gvariant_type}")
        if expected is not typ:
            raise TypeError(
                f"Expected {typ.__name__}, got GVariant type {gvariant_type}"
            )
    else:
        # GVariant omits the type annotation when unambiguous (e.g. '98.0' for double)
        value = parts[0]
    return typ(value)


def get_battery_info() -> tuple[float, BatteryState]:
    """Return (percentage, state) from UPower D-Bus properties."""
    pct = get_dbus_property("Percentage", float)
    state = BatteryState(get_dbus_property("State", int))
    return pct, state


def find_threshold(pct: float, thresholds: list[float]) -> float | None:
    """Return the lowest threshold >= pct, or None."""
    return next((t for t in thresholds if pct <= t), None)


def notify(pct: float) -> None:
    subprocess.run(
        ["notify-send", "-u", "critical", "Battery Low", f"Battery at {pct:.0f}%"],
        check=True,
    )


def check_battery(last_threshold: float | None) -> float | None:
    """Check battery and notify if a new threshold is crossed."""
    pct, state = get_battery_info()

    if state != BatteryState.DISCHARGING:
        return None

    current_threshold = find_threshold(pct, THRESHOLDS)
    if current_threshold is None:
        return last_threshold

    if last_threshold != current_threshold:
        notify(pct)
        return current_threshold

    return last_threshold


def main() -> None:
    last_threshold = check_battery(None)

    monitor = subprocess.Popen(
        [
            "gdbus",
            "monitor",
            "--system",
            "--dest",
            "org.freedesktop.UPower",
            "--object-path",
            DEVICE,
        ],
        stdout=subprocess.PIPE,
        text=True,
    )

    assert monitor.stdout is not None
    for line in monitor.stdout:
        last_threshold = check_battery(last_threshold)

    monitor.wait()
    if monitor.returncode != 0:
        raise Exception(f"gdbus monitor failed with code {monitor.returncode}")


if __name__ == "__main__":
    import sys

    if "--test" not in sys.argv:
        main()
        sys.exit()

    import unittest
    from unittest.mock import patch

    class TestGetDbusProperty(unittest.TestCase):
        def _mock_call(self, gdbus_output, typ, prop="Test"):
            with patch("subprocess.run") as mock_run:
                mock_run.return_value.stdout = gdbus_output
                return get_dbus_property(prop, typ)

        def test_double(self):
            assert self._mock_call("(<double 42.0>,)\n", float) == 42.0

        def test_double_without_annotation(self):
            assert self._mock_call("(<100.0>,)\n", float) == 100.0

        def test_uint32(self):
            assert self._mock_call("(<uint32 2>,)\n", int) == 2

        def test_type_mismatch(self):
            with self.assertRaises(TypeError):
                self._mock_call("(<double 42.0>,)\n", int)

        def test_unknown_gvariant_type(self):
            with self.assertRaises(ValueError):
                self._mock_call("(<faketype 99>,)\n", int)

    class TestFindThreshold(unittest.TestCase):
        def test_below_lowest(self):
            assert find_threshold(1.0, [3.0, 5.0, 10.0]) == 3.0

        def test_exact_match(self):
            assert find_threshold(5.0, [3.0, 5.0, 10.0]) == 5.0

        def test_between(self):
            assert find_threshold(4.0, [3.0, 5.0, 10.0]) == 5.0

        def test_above_all(self):
            assert find_threshold(50.0, [3.0, 5.0, 10.0]) is None

    class TestCheckBattery(unittest.TestCase):
        def _check(self, pct, state, last_threshold):
            with patch(__name__ + ".get_battery_info", return_value=(pct, state)):
                with patch(__name__ + ".notify") as mock_notify:
                    result = check_battery(last_threshold)
                    return result, mock_notify.called

        def test_above_thresholds(self):
            result, notified = self._check(50.0, BatteryState.DISCHARGING, None)
            assert result is None
            assert not notified

        def test_crosses_threshold(self):
            result, notified = self._check(9.0, BatteryState.DISCHARGING, None)
            assert result == 10.0
            assert notified

        def test_no_repeat(self):
            result, notified = self._check(8.0, BatteryState.DISCHARGING, 10.0)
            assert result == 10.0
            assert not notified

        def test_next_threshold(self):
            result, notified = self._check(4.0, BatteryState.DISCHARGING, 10.0)
            assert result == 5.0
            assert notified

        def test_charging_resets(self):
            result, notified = self._check(4.0, BatteryState.CHARGING, 5.0)
            assert result is None
            assert not notified

        def test_renotify_after_charge(self):
            result, notified = self._check(9.0, BatteryState.DISCHARGING, None)
            assert result == 10.0
            assert notified

    unittest.main(argv=["", "-v"])
