-- obsidian

-- A Neovim plugin for writing and navigating Obsidian vaults, written in Lua.

return {
  'epwalsh/obsidian.nvim',
  version = '*', -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = 'markdown.obsidian',
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  -- event = {
  --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
  --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
  --   -- refer to `:h file-pattern` for more examples
  --   "BufReadPre path/to/my-vault/*.md",
  --   "BufNewFile path/to/my-vault/*.md",
  -- },
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  init = function()
    -- which key extension
    local wk = require 'which-key'
    wk.add { '<leader>o', group = '[O]bsidian' }

    local obsidian_note_matcher = function()
      return function(path, bufnr)
        local lines = vim.api.nvim_buf_get_lines(bufnr, 1, 2, false)
        if lines[1] then
          if string.find(lines[1], 'id: ') ~= nil then
            return 'markdown.obsidian'
          else
            return 'markdown'
          end
        else
          return 'markdown'
        end
      end
    end

    vim.filetype.add {
      pattern = {
        ['.*.md'] = obsidian_note_matcher(),
      },
    }
  end,
  keys = {
    { '<leader>os', ':ObsidianSearch<CR>', mode = 'n', desc = 'Obsidian [S]earch' },
    { '<leader>on', ':ObsidianNew<CR>', mode = 'n', desc = 'Obsidian [N]ew' },
  },
  opts = {
    workspaces = {
      {
        name = 'primary',
        path = '~/Documents/vaults/primary',
      },
    },
    ui = {
      enable = true,
      checkboxes = {
        [' '] = { char = '󰄱', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '', hl_group = 'ObsidianDone' },
        ['!'] = { char = '', hl_group = 'ObsidianImportant' },
      },
    },
  },
}
