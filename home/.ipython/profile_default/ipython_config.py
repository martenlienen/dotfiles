import textwrap


def import_if_available(module, alias=None, setup=None):
    if alias:
        import_ = "{} as {}".format(module, alias)
    else:
        import_ = module

    if setup is None:
        setup = ""
    else:
        setup = textwrap.indent(setup, "    ")

    return """
try:
    import {}
{}
except ImportError:
    pass
""".format(import_, setup).strip()


c = get_config()
c.InteractiveShellApp.extensions = ["autoreload"]
c.InteractiveShellApp.exec_lines = [
    import_if_available("numpy", "np", setup="rng = np.random.default_rng()"),
    import_if_available("pandas", "pd"),
    import_if_available("matplotlib.pyplot", "pp"),
    import_if_available("torch"),
    "%autoreload 3"
]
c.TerminalInteractiveShell.confirm_exit = False
