try
    using OhMyREPL
catch e
    @warn(e)
end

try
    using Revise
catch e
    @warn(e)
end

try
    using PkgTemplates

    project = Template(
        user = "martenlienen",
        authors = "Marten Lienen <marten.lienen@gmail.com",
        julia = v"1.5",
        plugins = [!CompatHelper, !TagBot]
    )
catch e
    @warn(e)
end
