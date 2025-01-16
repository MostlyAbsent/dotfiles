-- todo-comments

-- todo-comments is a lua plugin for Neovim >= 0.8.0 to highlight and search
-- for todo comments like TODO, HACK, BUG in your code base.

return {
  'folke/todo-comments.nvim',
  event = 'VimEnter',
  dependencies = { 'nvim-lua/plenary.nvim' },
  opts = { signs = false },
  keys = { { '<leader>n', ':TodoQuickFix<CR>', mode = 'n', desc = '[n]OTES QuickFix List' } },
}
