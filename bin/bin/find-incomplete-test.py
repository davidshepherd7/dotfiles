#! /usr/bin/env python3

import sys
import argparse
from typing import Literal
from dataclasses import dataclass
import re


State = Literal["DONE", "STARTED"]


@dataclass(frozen=True)
class Result:
    name: str
    state: State


def parse_line(line: str) -> Result | None:
    if re.match("^WARNING", line):
        return None

    # Test names can contain some surprising characters!
    test_name_regex = r"unittests/[a-zA-Z0-9_/-]*\.py::[a-zA-Z0-9_:*]+(\[.*\])?+"

    m_started = re.match(rf"^({test_name_regex})", line)
    if m_started:
        return Result(name=m_started[1], state="STARTED")

    m_passed = re.match(
        rf".*(PASSED|SKIPPED|FAILED).*({test_name_regex}).*",
        line,
    )
    if m_passed:
        return Result(name=m_passed[2], state="DONE")

    print("Didn't parse line:", line)
    return None


def main(argv: list[str]) -> int:
    tests: dict[str, State] = {}

    for line in sys.stdin.readlines():
        r = parse_line(line.strip())
        if r is None:
            continue

        # Sanity check the state transition
        if r.state == "STARTED":
            print(tests)
            assert r.name not in tests, f"'{r.name}'"
        elif r.state == "DONE":
            print(tests)
            # assert r.name in tests, f"'{r.name}'"

        tests[r.name] = r.state

    pending_tests = [k for k, v in tests.items() if v == "STARTED"]
    print("\n\nTests pending at end of run:", pending_tests)

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
