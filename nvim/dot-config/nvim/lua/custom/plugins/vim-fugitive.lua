-- vim fugitive

-- Fugitive is the premier Vim plugin for Git. Or maybe it's the premier Git
-- plugin for Vim? Either way, it's "so awesome, it should be illegal". That's
-- why it's called Fugitive.

return {
  'tpope/vim-fugitive',
  init = function()
    local wk = require 'which-key'
    wk.add { '<leader>g', group = '[G]it' }
  end,
  keys = {
    { '<leader>g', group = '[G]it' },
    {
      '<leader>gs',
      ':Git status --long<CR>',
      mode = 'n',
      desc = 'Git [S]tatus',
    },
    {
      '<leader>gk',
      ':Git checkout ',
      mode = 'n',
      desc = 'Git chec[k]out',
    },
    {
      '<leader>gc',
      ':Git commit<CR>',
      mode = 'n',
      desc = 'Git [C]ommit',
    },
    {
      '<leader>ga',
      ':Git add --all<CR>',
      mode = 'n',
      desc = 'Git [A]dd all',
    },
    {
      '<leader>gp',
      ':Git push<CR>',
      mode = 'n',
      desc = 'Git [P]ush',
    },
  },
}
