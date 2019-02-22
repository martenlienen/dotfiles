def import_if_available(module, alias=None):
    if alias:
        import_ = "{} as {}".format(module, alias)
    else:
        import_ = module

    return """
try:
    import {}
except ImportError:
    pass
""".format(import_).strip()

c = get_config()
c.InteractiveShellApp.exec_lines = [
    import_if_available("numpy", "np"),
    import_if_available("matplotlib.pyplot", "pp")
]
