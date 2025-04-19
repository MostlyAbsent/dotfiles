-- mini.nvim

-- Library of 40+ independent Lua modules improving overall Neovim (version 0.8
-- and higher) experience with minimal effort. They all share same
-- configuration approaches and general design principles.

return {
  'echasnovski/mini.nvim',
  config = function()
    -- Better Around/Inside textobjects
    --
    -- Examples:
    --  - va)  - [V]isually select [A]round [)]paren
    --  - yinq - [Y]ank [I]nside [N]ext [']quote
    --  - ci'  - [C]hange [I]nside [']quote
    require('mini.ai').setup { n_lines = 500 }

    -- Add/delete/replace surroundings (brackets, quotes, etc.)
    --
    -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
    -- - sd'   - [S]urround [D]elete [']quotes
    -- - sr)'  - [S]urround [R]eplace [)] [']
    require('mini.surround').setup()

    require('mini.comment').setup()

    require('mini.jump2d').setup()

    require('mini.move').setup()

    require('mini.pairs').setup()

    require('mini.icons').setup()

    -- Simple and easy statusline.
    --  You could remove this setup call if you don't like it,
    --  and try some other statusline plugin
    -- local statusline = require 'mini.statusline'
    --
    -- -- set use_icons to true if you have a Nerd Font
    -- statusline.setup {
    --   use_icons = vim.g.have_nerd_font,
    --   content = {
    --     active = function()
    --       local mode, mode_hl = statusline.section_mode { trunc_width = 120 }
    --       local git = statusline.section_git { trunc_width = 40 }
    --       local diff = statusline.section_diff { trunc_width = 75 }
    --       local diagnostics = statusline.section_diagnostics { trunc_width = 75 }
    --       local lsp = statusline.section_lsp { trunc_width = 75 }
    --       local filename = statusline.section_filename { trunc_width = 140 }
    --       local fileinfo = statusline.section_fileinfo { trunc_width = 120 }
    --       local location = statusline.section_location { trunc_width = 75 }
    --       local search = statusline.section_searchcount { trunc_width = 75 }
    --
    --       local custom_filename = function(args)
    --         -- In terminal always use plain name
    --         if vim.bo.buftype == 'terminal' then
    --           return '%t'
    --         elseif statusline.is_truncated(args.trunc_width) then
    --           -- File name with 'truncate', 'modified', 'readonly' flags
    --           -- Use relative path if truncated
    --           if vim.bo.filetype == 'markdown' then
    --             return 'titty'
    --           else
    --             return '%f%m%r'
    --           end
    --         else
    --           -- Use fullpath if not truncated
    --           return '%F%m%r'
    --         end
    --       end
    --
    --       return statusline.combine_groups {
    --         { hl = mode_hl, strings = { mode } },
    --         -- { hl = 'MiniStatuslineDevinfo', strings = { git, diff, diagnostics, lsp } },
    --         { hl = 'MiniStatuslineDevinfo', strings = { git } },
    --         '%<',
    --         { hl = 'MiniStatuslineFilename', strings = { filename } },
    --         '%=',
    --         { hl = 'MiniStatuslineFileinfo', strings = { fileinfo } },
    --         { hl = mode_hl, strings = { search, location } },
    --       }
    --     end,
    --     inactive = nil,
    --   },
    -- }

    --   -- Usage of `MiniStatusline.combine_groups()` ensures highlighting and
    --   -- correct padding with spaces between groups (accounts for 'missing'
    --   -- sections, etc.)
    --   return MiniStatusline.combine_groups({
    --     { hl = mode_hl,                  strings = { mode } },
    --     { hl = 'MiniStatuslineDevinfo',  strings = { git, diff, diagnostics, lsp } },
    --     '%<', -- Mark general truncate point
    --     { hl = 'MiniStatuslineFilename', strings = { filename } },
    --     '%=', -- End left alignment
    --     { hl = 'MiniStatuslineFileinfo', strings = { fileinfo } },
    --     { hl = mode_hl,                  strings = { search, location } },
    --   })
    -- end

    -- You can configure sections in the statusline by overriding their
    -- default behavior. For example, here we set the section for
    -- cursor location to LINE:COLUMN
    -- ---@diagnostic disable-next-line: duplicate-set-field
    -- statusline.section_location = function()
    --   return '%2l:%-2v'
    -- end
  end,
}
