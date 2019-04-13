#!/usr/bin/env python3

import json
import os
import subprocess

SPOTIFY_PATH = "/usr/bin/spotify"
I3MSG_PATH = "/usr/bin/i3-msg"


def spotify_runs():
    pgrep = subprocess.run(["pgrep", "spotify"], stdout=subprocess.DEVNULL)
    return pgrep.returncode == 0


def start_spotify():
    os.execl(SPOTIFY_PATH, SPOTIFY_PATH)


class SpotifyException(Exception):
    """Yes, this is abused for a non-local return."""

    def __init__(self, on_scratchpad):
        self.on_scratchpad = on_scratchpad


def spotify_is_on_scratchpad():
    tree_cmd = [I3MSG_PATH, "-t", "get_tree"]
    tree_result = subprocess.run(tree_cmd, stdout=subprocess.PIPE)

    if tree_result.returncode != 0:
        raise Exception("Could not get i3's layout tree")

    def check_workspace(node, workspace=None):
        if node.get("type") == "workspace":
            workspace = node.get("name")

        if node.get("window_properties", {}).get("class") == "Spotify":
            # The scratchpad has a workspace name of __i3_scratch
            raise SpotifyException(workspace and "scratch" in workspace)
        else:
            if "nodes" in node:
                for child in node["nodes"]:
                    check_workspace(child, workspace)
            if "floating_nodes" in node:
                for child in node["floating_nodes"]:
                    check_workspace(child, workspace)

    tree = json.loads(tree_result.stdout)

    try:
        check_workspace(tree)

        raise Exception("Spotify window not found in the layout tree")
    except SpotifyException as e:
        return e.on_scratchpad


def show_spotify():
    subprocess.run([I3MSG_PATH, '[class="Spotify"] scratchpad show'])


def hide_spotify():
    subprocess.run([I3MSG_PATH, '[class="Spotify"] move scratchpad'])


def main():
    if spotify_runs():
        if spotify_is_on_scratchpad():
            show_spotify()
        else:
            hide_spotify()
    else:
        start_spotify()


if __name__ == "__main__":
    main()
