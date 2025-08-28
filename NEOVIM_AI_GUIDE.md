# Neovim Configuration AI Guide

> **Purpose**: This document helps AI agents understand Roi's Neovim configuration quickly and provide better assistance.

## Quick Overview

This is a **minimal, performance-focused, work-integrated** Neovim configuration designed for software development with specific integration for the Inspekto project. The setup prioritizes speed, maintainability, and practical workflow integration over feature maximalism.

**Key Philosophy**: Fast, focused, documented, and work-aware.

## Architecture & Organization

### File Structure
```
config/nvim/
├── init.lua                    # Entry point + embedded documentation
├── lua/roip/                   # Personal namespace (avoid conflicts)
│   ├── config/                 # Core Neovim configuration
│   │   ├── options.lua         # Editor options & performance settings
│   │   ├── keymaps.lua         # Key mappings & navigation
│   │   ├── autocommands.lua    # Auto commands
│   │   ├── toggles.lua         # Toggle functions
│   │   └── inspekto.lua        # Work-specific features
│   ├── plugins/                # Plugin configurations
│   │   ├── mini/               # Mini.nvim modules (modular approach)
│   │   ├── completion.lua      # Blink.cmp setup
│   │   ├── git.lua, tmux.lua, oil.lua, etc.
│   │   └── format.lua, lint.lua
│   ├── lsp.lua                 # LSP configuration & keybindings
│   └── colorscheme.lua         # Tokyo Night theme
├── lsp/                        # Individual LSP server configs
└── after/                      # Override configs
```

### Key Patterns
- **Namespace isolation**: Everything under `roip/` to avoid conflicts
- **Modular organization**: Each feature in separate files
- **Work integration**: Inspekto-specific code isolated but integrated
- **Documentation-first**: Extensive inline documentation
- **Performance-conscious**: Optimized settings throughout

## Plugin Stack & Rationale

### Core Philosophy: "Mini but Complete"
- **Curated selection**: Each plugin serves a clear, essential purpose
- **Modern alternatives**: Prefer newer, actively maintained plugins
- **Performance first**: No heavy or slow plugins
- **Consistent APIs**: Prefer plugins with similar configuration patterns

### Key Plugins
```lua
-- Package Management: Built-in vim.pack (no external manager)
-- Completion: blink.cmp (faster than nvim-cmp, better Lua integration)
-- LSP: Built-in vim.lsp + Mason (language server management)
-- File Management: oil.nvim (better than netrw, vim-native)
-- Git: Custom git plugin setup
-- UI: snacks.nvim (modern notifications/UI)
-- Utilities: mini.nvim (lightweight, consistent, well-maintained)
-- Theme: tokyonight.nvim (popular, well-supported)
```

### Why These Choices?
- **blink.cmp over nvim-cmp**: Significantly faster, written in Rust
- **oil.nvim over netrw**: Better UX, integrates with vim motions
- **mini.nvim over individual plugins**: Consistent API, lightweight, comprehensive
- **Built-in LSP**: Mature, fast, no external dependencies
- **vim.pack over lazy.nvim**: Simpler, faster startup, built-in

## Configuration Patterns

### Loading Order (Critical!)
```lua
-- In init.lua - ORDER MATTERS:
require("roip/config/options")     -- First: basic editor setup
require("roip/config/keymaps")     -- Then: key mappings
require("roip/config/autocommands") -- Then: auto commands
require("roip/config/toggles")     -- Then: toggle functions
require("roip/colorscheme")        -- Then: theme

-- Plugins in dependency order:
require("roip/plugins/tmux")       -- First: terminal integration
require("roip/plugins/git")        -- Then: git features
require("roip/plugins/treesitter") -- Then: syntax highlighting
require("roip/plugins/mason")      -- Then: LSP server management
require("roip/plugins/mini")       -- Then: utilities
require("roip/plugins/oil")        -- Then: file management
require("roip/plugins/snacks")     -- Then: UI components
require("roip/plugins/completion") -- Then: completion setup
require("roip/lsp")               -- AFTER completion (needs blink capabilities)
require("roip/plugins/format")     -- Then: formatting
require("roip/plugins/lint")       -- Then: linting
require("roip/plugins/copilot")    -- Finally: AI assistance

require("roip/config/inspekto")   -- Last: work-specific features
```

### Key Settings & Performance
```lua
-- Performance-critical settings:
vim.opt.updatetime = 250          -- Fast LSP/plugin responsiveness
vim.opt.swapfile = false          -- No swap files (using git + autowrite)
vim.opt.autowriteall = true       -- Seamless buffer switching
vim.opt.termguicolors = true      -- Modern color support
vim.g.mapleader = " "             -- Space leader (easier than backslash)

-- Coding preferences:
vim.opt.tabstop = 2               -- 2-space tabs
vim.opt.shiftwidth = 2            -- 2-space indentation
vim.opt.expandtab = true          -- Spaces not tabs
vim.opt.textwidth = 90            -- 90-character line limit
```

### Keybinding Patterns
```lua
-- Escape alternatives (muscle memory optimization):
-- jj, kk, jk, kj all mapped to <ESC> in insert mode

-- Leader-based workflows:
-- <Space> + letter = main actions
-- Movement optimization for wrapped lines
-- Alt + j/k = move lines up/down (works in all modes)

-- LSP bindings (following vim patterns):
-- K = hover documentation
-- gd = go to definition  
-- gr[n/a/r/i] = rename/actions/references/implementations
-- [d / ]d = previous/next diagnostic
```

## Work Integration (Inspekto)

### Auto-sync Feature
- **Trigger**: On file save (BufWritePost)
- **Condition**: File must be in `$INSPEKTO_PROJECT` directory
- **Action**: Rsync to remote development environment
- **Target**: `$INSPEKTO_REMOTE_PATH` (default: `roip@132.186.12.97:/home/roip/sinspekto/winspekto`)
- **Features**: Progress notifications, error handling, excludes build artifacts

### Environment Variables
```bash
export INSPEKTO_PROJECT="/path/to/local/project"
export INSPEKTO_REMOTE_PATH="user@host:/path/to/remote/project"
```

## LSP Configuration

### Architecture
1. **Language Servers**: Installed/managed by Mason
2. **Configurations**: Individual files in `lsp/` directory  
3. **Integration**: `lsp.lua` loads configs and sets up keybindings
4. **Capabilities**: Blink.cmp provides completion capabilities

### Supported Languages
Based on Mason setup and LSP configs:
- **Python**: pyright, ruff (formatting/linting)
- **JavaScript/TypeScript**: typescript-language-server, eslint
- **Lua**: lua_ls
- **And others** (check `lsp/` directory for full list)

### Key LSP Keybindings
```
K           = hover documentation
gd          = go to definition
grn         = rename symbol
gra         = code actions  
grr         = list references
gri         = list implementations
gO          = document symbols
Ctrl+s      = signature help (insert mode)
[d / ]d     = navigate diagnostics
Ctrl+w+d    = show diagnostic details
```

## Common Modification Patterns

### Adding a New Language
1. **Install language server**: Add to Mason's `ensure_installed` in `mason.lua`
2. **Create LSP config**: New file in `lsp/` directory (copy from nvim-lspconfig examples)
3. **Register server**: Add to `lsp_servers` list in `lsp.lua`
4. **Add formatting**: Update `format.lua` if formatter available
5. **Add linting**: Update `lint.lua` if linter available

### Adding a New Plugin
1. **Create plugin file**: New file in `lua/roip/plugins/`
2. **Add package**: `vim.pack.add({ "author/plugin-name" })` 
3. **Configure plugin**: Setup in the same file
4. **Load plugin**: Add `require("roip/plugins/new-plugin")` to `init.lua`
5. **Maintain order**: Respect dependency loading order

### Modifying Keybindings
1. **Check existing**: Look in `keymaps.lua` first
2. **Follow patterns**: Use leader for main actions, follow vim conventions
3. **Document purpose**: Add description to keymap calls
4. **Test conflicts**: Ensure no conflicts with plugin bindings

### Work-specific Features
- **All code goes in**: `lua/roip/config/inspekto.lua`
- **Environment-driven**: Use env vars for configuration
- **Non-intrusive**: Should not affect general Neovim usage
- **Well-documented**: Explain work-specific context

## Troubleshooting Guide

### LSP Issues
```lua
-- Diagnostic commands:
:LspInfo                 -- Check active LSP clients
:Mason                   -- Check installed language servers  
:LspLog                  -- View LSP logs
:lua =vim.lsp.get_active_clients()  -- Debug LSP clients
```

### Completion Issues
- **Check blink.cmp**: Ensure it loads before LSP setup
- **Check capabilities**: Verify LSP servers have completion capabilities
- **Check sources**: `:lua =require('blink.cmp').config.sources`

### Performance Issues
```lua
-- Profile startup:
nvim --startuptime startup.log

-- Check updatetime:
:lua =vim.opt.updatetime:get()

-- Profile runtime:
:profile start profile.log
:profile func *
:profile file *
-- do some work
:profile pause
:noautocmd qall!
```

### Plugin Issues
- **Check loading order**: Ensure dependencies load first
- **Check vim.pack**: `:lua =vim.pack.list()` to see loaded packages
- **Check after/**: Look for conflicting overrides

## AI Agent Guidelines

### When Suggesting Changes:

#### DO:
- **Maintain namespace**: Keep everything under `roip/` 
- **Respect loading order**: Understand dependency chain in `init.lua`
- **Follow patterns**: Match existing code style and organization
- **Consider performance**: This is a fast, minimal config
- **Preserve documentation**: Maintain inline documentation style
- **Check work integration**: Consider Inspekto workflow impact

#### DON'T:
- **Break loading order**: Don't suggest moving LSP before completion
- **Add heavy plugins**: Avoid slow or bloated plugins
- **Ignore namespace**: Don't suggest global modifications
- **Remove documentation**: Keep embedded docs in init.lua
- **Overcomplicate**: Prefer simple, direct solutions

#### Common Request Patterns:
- **"Add language support"** → Update Mason, create LSP config, possibly format/lint
- **"Add plugin"** → Create in plugins/, add to init.lua, respect order  
- **"Fix keybinding"** → Check keymaps.lua, consider existing patterns
- **"Performance issue"** → Check options.lua, look for conflicting autocmds
- **"Work feature"** → Add to inspekto.lua, use environment variables

#### This User Prefers:
- **Minimal solutions**: Don't suggest feature-heavy alternatives
- **Documentation**: Explain why, not just how
- **Performance awareness**: Consider startup time and runtime speed
- **Practical focus**: Work-oriented, real-world solutions
- **Maintainable code**: Clear, organized, well-commented

### Understanding Context:
- **This is a senior developer's config**: Expect sophisticated requirements
- **Work-focused**: Development workflow optimization is important  
- **Performance-conscious**: Speed matters more than features
- **Documentation-driven**: Code should be self-explaining
- **Tool integration**: Tmux, git, work environment matter

## Quick Reference

### File Locations:
- **Main config**: `config/nvim/init.lua`
- **Options**: `config/nvim/lua/roip/config/options.lua`  
- **Keymaps**: `config/nvim/lua/roip/config/keymaps.lua`
- **LSP setup**: `config/nvim/lua/roip/lsp.lua`
- **Completion**: `config/nvim/lua/roip/plugins/completion.lua`
- **Work features**: `config/nvim/lua/roip/config/inspekto.lua`

### Key Commands:
- **Reload config**: Source init.lua (or restart nvim)
- **Check health**: `:checkhealth`
- **LSP info**: `:LspInfo`
- **Mason**: `:Mason`
- **Package list**: `:lua =vim.pack.list()`

---

**Last Updated**: August 2025  
**Neovim Version**: 0.10+  
**Configuration Owner**: Roi Perelman  
**Purpose**: AI agent assistance and configuration understanding
