local wezterm = require("wezterm")

Config = wezterm.config_builder()

Config.automatically_reload_config = true
-- Config.enable_tab_bar = false
Config.window_close_confirmation = "NeverPrompt"
Config.window_decorations = "RESIZE" -- disable the title bar but enable resize
Config.window_padding = { left = 8, right = 8, top = 0, bottom = 0, }
Config.color_scheme = "Dracula (Official)"
Config.font = wezterm.font("JetBrains Mono", { weight = "Bold" })
Config.font_size = 13

-- tab bar
Config.hide_tab_bar_if_only_one_tab = false
Config.tab_bar_at_bottom = true
Config.use_fancy_tab_bar = false
Config.tab_max_width = 25

-- tmux
Config.leader = { key = "a", mods = "CTRL" }
Config.keys = {
  { key = "a",  mods = "LEADER|CTRL",  action = wezterm.action { SendString = "\x01" } },
  { key = "c",  mods = "LEADER",       action = wezterm.action { SpawnTab = "CurrentPaneDomain" } },
  { key = "x",  mods = "LEADER",       action = wezterm.action { CloseCurrentPane = { confirm = true } } },
  { key = "\"", mods = "LEADER|SHIFT", action = wezterm.action { SplitVertical = { domain = "CurrentPaneDomain" } } },
  { key = "%",  mods = "LEADER|SHIFT", action = wezterm.action { SplitHorizontal = { domain = "CurrentPaneDomain" } } },
  { key = "s",  mods = "LEADER",       action = wezterm.action { SplitVertical = { domain = "CurrentPaneDomain" } } },
  { key = "v",  mods = "LEADER",       action = wezterm.action { SplitHorizontal = { domain = "CurrentPaneDomain" } } },
  { key = "z",  mods = "LEADER",       action = "TogglePaneZoomState" },
  { key = "h",  mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Left" } },
  { key = "j",  mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Down" } },
  { key = "k",  mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Up" } },
  { key = "l",  mods = "LEADER",       action = wezterm.action { ActivatePaneDirection = "Right" } },
  -- { key = "h",  mods = "CTRL",         action = wezterm.action { ActivatePaneDirection = "Left" } },
  -- { key = "j",  mods = "CTRL",         action = wezterm.action { ActivatePaneDirection = "Down" } },
  -- { key = "k",  mods = "CTRL",         action = wezterm.action { ActivatePaneDirection = "Up" } },
  -- { key = "l",  mods = "CTRL",         action = wezterm.action { ActivatePaneDirection = "Right" } },
  { key = "h",  mods = "ALT",          action = wezterm.action { AdjustPaneSize = { "Left", 5 } } },
  { key = "j",  mods = "ALT",          action = wezterm.action { AdjustPaneSize = { "Down", 5 } } },
  { key = "k",  mods = "ALT",          action = wezterm.action { AdjustPaneSize = { "Up", 5 } } },
  { key = "l",  mods = "ALT",          action = wezterm.action { AdjustPaneSize = { "Right", 5 } } },
  { key = "1",  mods = "LEADER",       action = wezterm.action { ActivateTab = 0 } },
  { key = "2",  mods = "LEADER",       action = wezterm.action { ActivateTab = 1 } },
  { key = "3",  mods = "LEADER",       action = wezterm.action { ActivateTab = 2 } },
  { key = "4",  mods = "LEADER",       action = wezterm.action { ActivateTab = 3 } },
  { key = "5",  mods = "LEADER",       action = wezterm.action { ActivateTab = 4 } },
  { key = "6",  mods = "LEADER",       action = wezterm.action { ActivateTab = 5 } },
  { key = "7",  mods = "LEADER",       action = wezterm.action { ActivateTab = 6 } },
  { key = "8",  mods = "LEADER",       action = wezterm.action { ActivateTab = 7 } },
  { key = "9",  mods = "LEADER",       action = wezterm.action { ActivateTab = 8 } },
}

return Config
