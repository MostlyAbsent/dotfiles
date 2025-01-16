-- vim table mode

-- An awesome automatic table creator & formatter allowing one to create neat
-- tables as you type.

return {
  'dhruvasagar/vim-table-mode',
  init = function()
    local wk = require 'which-key'
    wk.add { '<leader>t', group = '[T]able Mode' }
  end,
  keys = {
    { '<leader>tt', ':TableModeToggle<CR>', mode = 'n', desc = 'Table Mode [T]oggle' },
    { '<leader>tr', ':TableModeRealign<CR>', mode = 'n', desc = 'Table Mode [R]ealign' },
  },
}
