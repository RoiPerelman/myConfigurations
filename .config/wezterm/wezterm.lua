local wezterm = require("wezterm")

Config = wezterm.config_builder()

Config.automatically_reload_config = true
Config.enable_tab_bar = false
Config.window_close_confirmation = "NeverPrompt"
Config.window_decorations = "RESIZE" -- disable the title bar but enable resize
Config.color_scheme = "Dracula (Official)"
Config.font = wezterm.font("JetBrains Mono", {weight = "Bold"})
Config.font_size = 13

return Config
