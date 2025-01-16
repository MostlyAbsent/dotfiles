-- markdown

-- Configurable tools for working with markdown files in Neovim.

return {
  'tadmccorkle/markdown.nvim',
  ft = { 'markdown.obsidian', 'markdown.mdx', 'markdown', 'marksman' },
  keys = {
    {
      '<M-CR>',
      function()
        require('markdown.list').insert_list_item_below()
      end,
      mode = { 'n', 'i' },
      desc = 'MDListItemBelow',
    },
  },
  opts = {},
}
