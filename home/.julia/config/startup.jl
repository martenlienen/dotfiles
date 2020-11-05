macro try_import(pkg)
    quote
        try
            using $pkg
        catch e
            @warn(e)
        end
    end
end

@try_import OhMyREPL
@try_import Revise
@try_import PkgTemplates

if isdefined(Main, :PkgTemplates)
    project = Template(
        user = "martenlienen",
        authors = "Marten Lienen <marten.lienen@gmail.com>",
        julia = v"1.5",
        plugins = [!CompatHelper, !TagBot]
    )
end
