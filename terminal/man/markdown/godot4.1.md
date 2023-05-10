Godot Engine v4.0.2.stable.official.7a0977ce2 - https://godotengine.org
Free and open source software under the terms of the MIT license.
(c) 2014-present Godot Engine contributors.
(c) 2007-2014 Juan Linietsky, Ariel Manzur.

Usage: godot [options] [path to scene or 'project.godot' file]

General options:
  -h, --help                        Display this help message.
  --version                         Display the version string.
  -v, --verbose                     Use verbose stdout mode.
  -q, --quiet                       Quiet mode, silences stdout messages. Errors are still displayed.

Run options:
  --, ++                            Separator for user-provided arguments. Following arguments are not used by the engine, but can be read from `OS.get_cmdline_user_args()`.
  -e, --editor                      Start the editor instead of running the scene.
  -p, --project-manager             Start the project manager, even if a project is auto-detected.
  --debug-server <uri>              Start the editor debug server (<protocol>://<host/IP>[:<port>], e.g. tcp://127.0.0.1:6007)
  --quit                            Quit after the first iteration.
  -l, --language <locale>           Use a specific locale (<locale> being a two-letter code).
  --path <directory>                Path to a project (<directory> must contain a 'project.godot' file).
  -u, --upwards                     Scan folders upwards for project.godot file.
  --main-pack <file>                Path to a pack (.pck) file to load.
  --render-thread <mode>            Render thread mode ['unsafe', 'safe', 'separate'].
  --remote-fs <address>             Remote filesystem (<host/IP>[:<port>] address).
  --remote-fs-password <password>   Password for remote filesystem.
  --audio-driver <driver>           Audio driver ['CoreAudio', 'Dummy'].
  --display-driver <driver>         Display driver (and rendering driver) ['macos' ('vulkan', 'opengl3'), 'headless' ('dummy')].
  --rendering-method <renderer>     Renderer name. Requires driver support.
  --rendering-driver <driver>       Rendering driver (depends on display driver).
  --gpu-index <device_index>        Use a specific GPU (run with --verbose to get available device list).
  --text-driver <driver>            Text driver (Fonts, BiDi, shaping).
  --tablet-driver <driver>          Pen tablet input driver.
  --headless                        Enable headless mode (--display-driver headless --audio-driver Dummy). Useful for servers and with --script.
  --write-movie <file>              Writes a video to the specified path (usually with .avi or .png extension).
                                    --fixed-fps is forced when enabled, but it can be used to change movie FPS.
                                    --disable-vsync can speed up movie writing but makes interaction more difficult.

Display options:
  -f, --fullscreen                  Request fullscreen mode.
  -m, --maximized                   Request a maximized window.
  -w, --windowed                    Request windowed mode.
  -t, --always-on-top               Request an always-on-top window.
  --resolution <W>x<H>              Request window resolution.
  --position <X>,<Y>                Request window position (if set, screen argument is ignored).
  --screen <N>                      Request window screen.
  --single-window                   Use a single window (no separate subwindows).
    --xr-mode <mode>                  Select XR (Extended Reality) mode ['default', 'off', 'on'].

Debug options:
  -d, --debug                       Debug (local stdout debugger).
  -b, --breakpoints                 Breakpoint list as source::line comma-separated pairs, no spaces (use %20 instead).
  --profiling                       Enable profiling in the script debugger.
  --gpu-profile                     Show a GPU profile of the tasks that took the most time during frame rendering.
  --gpu-validation                  Enable graphics API validation layers for debugging.
  --gpu-abort                       Abort on graphics API usage errors (usually validation layer errors). May help see the problem if your system freezes.
  --remote-debug <uri>              Remote debug (<protocol>://<host/IP>[:<port>], e.g. tcp://127.0.0.1:6007).
  --debug-collisions                Show collision shapes when running the scene.
  --debug-paths                     Show path lines when running the scene.
  --debug-navigation                Show navigation polygons when running the scene.
  --debug-stringnames               Print all StringName allocations to stdout when the engine quits.
  --frame-delay <ms>                Simulate high CPU load (delay each frame by <ms> milliseconds).
  --time-scale <scale>              Force time scale (higher values are faster, 1.0 is normal speed).
  --disable-vsync                   Forces disabling of vertical synchronization, even if enabled in the project settings. Does not override driver-level V-Sync enforcement.
  --disable-render-loop             Disable render loop so rendering only occurs when called explicitly from script.
  --disable-crash-handler           Disable crash handler when supported by the platform code.
  --fixed-fps <fps>                 Force a fixed number of frames per second. This setting disables real-time synchronization.
  --print-fps                       Print the frames per second to the stdout.

Standalone tools:
  -s, --script <script>             Run a script.
  --check-only                      Only parse for errors and quit (use with --script).
  --export-release <preset> <path>  Export the project in release mode using the given preset and output path. The preset name should match one defined in export_presets.cfg.
                                    <path> should be absolute or relative to the project directory, and include the filename for the binary (e.g. 'builds/game.exe').
                                    The target directory must exist.
  --export-debug <preset> <path>    Export the project in debug mode using the given preset and output path. See --export-release description for other considerations.
  --export-pack <preset> <path>     Export the project data only using the given preset and output path. The <path> extension determines whether it will be in PCK or ZIP format.
  --convert-3to4 [<max_file_kb>] [<max_line_size>]
                                    Converts project from Godot 3.x to Godot 4.x.
  --validate-conversion-3to4 [<max_file_kb>] [<max_line_size>]
                                    Shows what elements will be renamed when converting project from Godot 3.x to Godot 4.x.
  --doctool [<path>]                Dump the engine API reference to the given <path> (defaults to current dir) in XML format, merging if existing files are found.
  --no-docbase                      Disallow dumping the base types (used with --doctool).
  --build-solutions                 Build the scripting solutions (e.g. for C# projects). Implies --editor and requires a valid project to edit.
  --dump-gdextension-interface      Generate GDExtension header file 'gdextension_interface.h' in the current folder. This file is the base file required to implement a GDExtension.
  --dump-extension-api              Generate JSON dump of the Godot API for GDExtension bindings named 'extension_api.json' in the current folder.
  --startup-benchmark               Benchmark the startup time and print it to console.
  --startup-benchmark-file <path>   Benchmark the startup time and save it to a given file in JSON format.
