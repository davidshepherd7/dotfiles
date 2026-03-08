#!/usr/bin/env python3
"""Event-driven battery notifications via UPower DBus.

Run tests: ./battery-notify.py --test
Type check: pipx run mypy battery-notify.py
"""

import subprocess
import re

# Must be sorted low to high
THRESHOLDS = [3, 5, 10]
DEVICE = "/org/freedesktop/UPower/devices/DisplayDevice"


def parse_upower(output: str) -> tuple[int, str]:
    """Extract percentage and state from upower -i output."""
    pct_match = re.search(r"percentage:\s*(\d+)%", output)
    state_match = re.search(r"state:\s*(\S+)", output)
    assert pct_match and state_match, f"Failed to parse upower output: {output}"
    pct = int(pct_match.group(1))
    state = state_match.group(1)
    return pct, state


def find_threshold(pct: int, thresholds: list[int]) -> int | None:
    """Return the lowest threshold >= pct, or None."""
    return next((t for t in thresholds if pct <= t), None)


def get_battery_info() -> str:
    return subprocess.run(
        ["upower", "-i", DEVICE], capture_output=True, text=True
    ).stdout


def notify(pct: int) -> None:
    subprocess.run(
        ["notify-send", "-u", "critical", "Battery Low", f"Battery at {pct}%"]
    )


def check_battery(last_threshold: int | None) -> int | None:
    """Check battery and notify if a new threshold is crossed.

    Returns the new last_threshold value.
    """
    pct, state = parse_upower(get_battery_info())

    if state != "discharging":
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


if __name__ == "__main__":
    import sys

    if "--test" not in sys.argv:
        main()
        sys.exit()

    import unittest
    from unittest.mock import patch

    class TestParseUpower(unittest.TestCase):
        def test_parse(self):
            output = "    state:               discharging\n    percentage:          42%\n"
            assert parse_upower(output) == (42, "discharging")

        def test_parse_charging(self):
            output = (
                "    state:               charging\n    percentage:          87%\n"
            )
            assert parse_upower(output) == (87, "charging")

    class TestFindThreshold(unittest.TestCase):
        def test_below_lowest(self):
            assert find_threshold(1, [3, 5, 10]) == 3

        def test_exact_match(self):
            assert find_threshold(5, [3, 5, 10]) == 5

        def test_between(self):
            assert find_threshold(4, [3, 5, 10]) == 5

        def test_above_all(self):
            assert find_threshold(50, [3, 5, 10]) is None

    class TestCheckBattery(unittest.TestCase):
        def _check(self, pct, state, last_threshold):
            output = f"    state:               {state}\n    percentage:          {pct}%\n"
            with patch(__name__ + ".get_battery_info", return_value=output):
                with patch(__name__ + ".notify") as mock_notify:
                    result = check_battery(last_threshold)
                    return result, mock_notify.called

        def test_above_thresholds(self):
            result, notified = self._check(50, "discharging", None)
            assert result is None
            assert not notified

        def test_crosses_threshold(self):
            result, notified = self._check(9, "discharging", None)
            assert result == 10
            assert notified

        def test_no_repeat(self):
            result, notified = self._check(8, "discharging", 10)
            assert result == 10
            assert not notified

        def test_next_threshold(self):
            result, notified = self._check(4, "discharging", 10)
            assert result == 5
            assert notified

        def test_charging_resets(self):
            result, notified = self._check(4, "charging", 5)
            assert result is None
            assert not notified

        def test_renotify_after_charge(self):
            result, notified = self._check(9, "discharging", None)
            assert result == 10
            assert notified

    unittest.main(argv=["", "-v"])
