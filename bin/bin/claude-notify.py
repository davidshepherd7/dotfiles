#! /usr/bin/env python3

import json
import subprocess
import sys
from typing import Literal, NotRequired, TypedDict


class Notification(TypedDict):
    # Common fields (all hook types)
    session_id: str
    transcript_path: str
    cwd: str
    permission_mode: Literal[
        "default", "plan", "acceptEdits", "dontAsk", "bypassPermissions"
    ]
    hook_event_name: str
    # Notification-specific fields
    message: str
    title: NotRequired[str]
    notification_type: Literal[
        "permission_prompt", "idle_prompt", "auth_success", "elicitation_dialog"
    ]
    # Optional subagent fields
    agent_id: NotRequired[str]
    agent_type: NotRequired[str]


def parse_notification() -> Notification:
    data: Notification = json.load(sys.stdin)
    return data


Urgency = Literal["low", "normal", "critical"]


def notify(
    title: str,
    message: str,
    expire_time_ms: int | None = None,
    urgency: Urgency = "normal",
    wait: bool = False,
) -> None:
    subprocess.run(
        [
            "notify-send",
            "--app-name", "Claude Code",
            "--urgency", urgency,
            *(["--expire-time", str(expire_time_ms)] if expire_time_ms is not None else []),
            *(["--wait"] if wait else []),
            title,
            message,
        ],
        check=True,
    )


def main():
    notification = parse_notification()
    title = notification.get("title") or "Claude Code"
    message = notification["message"]
    agent_type = notification.get("agent_type")
    extra = f"\n<small>({agent_type})</small>" if agent_type else ""
    notify(title, message + extra)
    return 0


# If this script is run from a shell then run main() and return the result.
if __name__ == "__main__":
    sys.exit(main())
