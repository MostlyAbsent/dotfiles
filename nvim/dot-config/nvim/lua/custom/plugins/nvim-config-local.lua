-- config local

-- Vim provides a feature called exrc, which allows to use config files that
-- are local to the current working directory. However, unconditionally
-- sourcing whatever files we might have in our current working directory can
-- be potentially dangerous. Because of that, neovim has disabled the feature.
-- (reenabled from version 0.9) The plugin tries to solve this issue by keeping
-- track of file hashes and allowing only trusted files to be sourced

return {
  'klen/nvim-config-local',
  event = 'InsertEnter',
}
