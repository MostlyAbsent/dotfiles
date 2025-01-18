return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim', -- required
    'sindrets/diffview.nvim', -- optional - Diff integration

    -- Only one of these is needed.
    'nvim-telescope/telescope.nvim', -- optional
  },
  keys = {
    {
      '<leader>g',
      ':Neogit<CR>',
      mode = 'n',
      desc = 'Git [S]tatus',
    },
  },
  config = true,
}
