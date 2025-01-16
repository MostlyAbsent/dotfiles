-- harpoon2

-- Getting you where you want with the fewest keystrokes.

return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local harpoon = require 'harpoon'
    ---@diagnostic disable-next-line: missing-parameter
    harpoon:setup()
    local function map(lhs, rhs, opts)
      vim.keymap.set('n', lhs, rhs, opts or {})
    end
    map('<leader>a', function()
      harpoon:list():append()
    end)
    map('<c-e>', function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end)
    map('<c-h>', function()
      harpoon:list():select(1)
      vim.api.nvim_feedkeys('zt', 'n', false)
    end)
    map('<c-t>', function()
      harpoon:list():select(2)
      vim.api.nvim_feedkeys('zt', 'n', false)
    end)
    map('<c-n>', function()
      harpoon:list():select(3)
      vim.api.nvim_feedkeys('zt', 'n', false)
    end)
    map('<c-s>', function()
      harpoon:list():select(4)
      vim.api.nvim_feedkeys('zt', 'n', false)
    end)
  end,
}
