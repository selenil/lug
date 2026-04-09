# Lug

An implementation of the Lua VM in pure Gleam.

> [!NOTE]
> This project is still early in development and thus it is not published in Hex. If you want to try it as it is now, 
> you can add it to a Gleam project as a git dependency by putting this line `lug = { git = "git@github.com:selenil/lug.git", ref = "main" }` 
> under the `dependencies` section in your `gleam.toml` file. 
> 
> In case you are looking for a library to embed Lua in Gleam programs, check out 
> [glua](https://github.com/selenil/glua), which uses the erlang library [luerl](https://github.com/rvirding/luerl) 
> as the Lua backend and it is a more mature project, just keep in mind that `glua` will eventually 
> switch its Lua backend to `lug` when `lug` reaches a stable release.
