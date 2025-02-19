-- harpoon2

-- Getting you where you want with the fewest keystrokes.

return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local harpoon = require 'harpoon'

    local custom_config = require('harpoon.config').merge_config {
      default = {
        display = function(list_item)
          local ft = vim.filetype.match { filename = list_item.value }
          if ft == 'markdown' then
            local function file_exists(file)
              local f = io.open(file, 'r')
              if f then
                f:close()
              end
              return f ~= nil
            end

            local function lines_from(file, count)
              if not file_exists(file) then
                return {}
              end

              local lines = {}

              for line in io.lines(file) do
                lines[#lines + 1] = line
                if #lines == count then
                  break
                end
              end

              return lines
            end

            local header = lines_from(list_item.value, 5)

            if header[2] then
              if string.find(header[2], 'id: ') then
                return string.sub(header[4], 5)
              end
            else
              return list_item.value
            end
          end

          return list_item.value
        end,
      },
    }

    ---@diagnostic disable-next-line:param-type-mismatch
    harpoon:setup(custom_config)
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
