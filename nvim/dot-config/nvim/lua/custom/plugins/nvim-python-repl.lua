-- python repl

-- A simple plugin that leverages treesitter to send expressions, statements,
-- function definitions and class definitions to a REPL.
--
-- The plugin now supports three different filetypes: python, scala and lua. It
-- is suggested that you have ipython, sbt, and ilua installed in your path
-- respectively. (Scala projects are expecting that the scala file is opened
-- from the directory containing build.sbt).

return {
  'geg2102/nvim-python-repl',
  dependencies = 'nvim-treesitter',
  ft = {
    'python',
    'lua',
    'scala',
  },
  config = function()
    require('nvim-python-repl').setup { execute_on_send = false, vsplit = false }
    -- vim.keymap.set("n", [your keymap], function()
    -- require('nvim-python-repl').send_statement_definition() end, { desc =
    -- "Send semantic unit to REPL"})

    vim.keymap.set('v', '<leader>ce', function()
      require('nvim-python-repl').send_visual_to_repl()
    end, { desc = 'Send visual selection to REPL' })

    -- vim.keymap.set("n", [your keyamp], function() require('nvim-python-repl').send_buffer_to_repl() end, { desc = "Send entire buffer to REPL"})

    -- vim.keymap.set("n", [your keymap], function() require('nvim-python-repl').toggle_execute() end, { desc = "Automatically execute command in REPL after sent"})

    -- vim.keymap.set("n", [your keymap], function() require('nvim-python-repl').toggle_vertical() end, { desc = "Create REPL in vertical or horizontal split"})

    -- vim.keymap.set("n", [your keymap], function() require('nvim-python-repl').open_repl() end, { desc = "Opens the REPL in a window split"})
  end,
}
