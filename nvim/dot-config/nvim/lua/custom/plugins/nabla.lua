-- nabla

-- Take your scentific notes in Neovim.

return {
  'jbyuki/nabla.nvim',
  ft = 'markdown',
  config = function()
    require('nabla').enable_virt()
  end,
  -- keys = {
  --   { 'n', '<cmd>lua require("nabla").popup()<CR>', '[P]review math' },
  -- },
}
