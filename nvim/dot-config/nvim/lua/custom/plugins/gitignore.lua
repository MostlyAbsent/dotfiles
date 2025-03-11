return {
  'wintermute-cell/gitignore.nvim',
  config = function()
    require 'gitignore'
  end,
  keys = {
    {
      '<leader>gi',
      ':Gitignore<CR>',
      mode = 'n',
      desc = 'Git [I]gnore Generator',
    },
  },
}
