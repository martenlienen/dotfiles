#!/usr/bin/env python3

import json
import shutil
import subprocess
from argparse import ArgumentParser
from dataclasses import dataclass

I3MSG_PATH = "/usr/bin/swaymsg"


def i3_layout_tree():
    tree_cmd = [I3MSG_PATH, "-t", "get_tree"]
    tree_result = subprocess.run(tree_cmd, stdout=subprocess.PIPE)

    if tree_result.returncode != 0:
        raise Exception("Could not get i3's layout tree")
    else:
        return json.loads(tree_result.stdout)


class WorkspaceException(Exception):
    """Yes, this is abused for a non-local return."""

    def __init__(self, workspace, node):
        self.workspace = workspace
        self.node = node


@dataclass
class WindowFilter:
    app_id: str | None
    window_class: str | None

    def matches(self, node: dict):
        if self.app_id is not None and node.get("app_id") == self.app_id:
            return True
        class_name = node.get("window_properties", {}).get("class")
        if self.window_class is not None and class_name == self.window_class:
            return True
        return False

    def __str__(self):
        parts = []
        if self.app_id is not None:
            parts.append(f"app_id={self.app_id}")
        if self.window_class is not None:
            parts.append(f'class="(?i){self.window_class}"')
        return ",".join(parts)


def find_program_workspace_and_node(filter):
    def search(node, workspace=None):
        if node.get("type") == "workspace":
            workspace = node

        if filter.matches(node):
            raise WorkspaceException(workspace, node)
        else:
            if "nodes" in node:
                for child in node["nodes"]:
                    search(child, workspace)
            if "floating_nodes" in node:
                for child in node["floating_nodes"]:
                    search(child, workspace)

    try:
        layout_tree = i3_layout_tree()
        search(layout_tree)

        raise Exception(f"[{filter}] window not found in the layout tree")
    except WorkspaceException as e:
        return e.workspace, e.node


def workspace_contains_focus(workspace):
    class FoundIt(Exception):
        pass

    # Check if the workspace contains a focused window
    def search(node):
        is_focused = node.get("focused", False)
        if is_focused:
            raise FoundIt()

        if "nodes" in node:
            for child in node["nodes"]:
                search(child)
        if "floating_nodes" in node:
            for child in node["floating_nodes"]:
                search(child)

    try:
        search(workspace)

        return False
    except FoundIt:
        return True


def program_runs(program):
    # We match against the process name, so that we don't match toggle_window itself.
    # However, the process name is at most 16 null-terminated bytes [1], so we only
    # search for the first 15 characters.
    #
    # [1] https://stackoverflow.com/questions/23534263/what-is-the-maximum-allowed-limit-on-the-length-of-a-process-name
    pgrep = subprocess.run(["pgrep", program[:15]], stdout=subprocess.DEVNULL)
    return pgrep.returncode == 0


def start_program(program, args: list[str]):
    path = shutil.which(program)
    subprocess.Popen([path] + args, start_new_session=True)


def program_is_on_scratchpad(workspace):
    # The scratchpad has a workspace name of __i3_scratch
    return "scratch" in workspace.get("name", "")


def show_program(filter, width=None, height=None):
    cmd = f"[{filter}] scratchpad show; floating enable; "
    if width is not None:
        cmd += f"resize set width {width}px; "
    if height is not None:
        cmd += f"resize set height {height}px; "
    cmd += "move position center"
    subprocess.run([I3MSG_PATH, cmd])


def hide_program(filter):
    subprocess.run([I3MSG_PATH, f"[{filter}] move scratchpad"])


def focus_program(filter):
    subprocess.run([I3MSG_PATH, f"[{filter}] focus"])


def main():
    parser = ArgumentParser(description="Toggle a window to and from the scratchpad")
    parser.add_argument("--app-id", help="App ID of the window")
    parser.add_argument("--class", dest="window_class", help="WM_CLASS of the window")
    parser.add_argument("--width", type=int, help="Resize window to this width")
    parser.add_argument("--height", type=int, help="Resize window to this height")
    parser.add_argument("program", nargs="+", help="Program and options")
    args = parser.parse_args()

    program, *program_args = args.program
    app_id = args.app_id
    window_class = args.window_class
    width, height = args.width, args.height

    if app_id is None and window_class is None:
        print("Either --app-id or --class are required")
        raise SystemExit()

    filter = WindowFilter(app_id, window_class)

    if program_runs(program):
        workspace, node = find_program_workspace_and_node(filter)

        if workspace_contains_focus(workspace) and node["focused"]:
            hide_program(filter)
        else:
            # If the program is on another workspace, move it to the scratch pad
            if not program_is_on_scratchpad(workspace):
                hide_program(filter)

            # Now move it from the scratchpad to the current workspace
            show_program(filter, width, height)

            # Finally, focus it to raise it to the front.
            focus_program(filter)
    else:
        start_program(program, program_args)


if __name__ == "__main__":
    main()
