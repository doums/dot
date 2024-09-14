---@diagnostic disable
local xplr = xplr
---@diagnostic enable

local M = {}

M.is_executable = function(a)
  if a.is_dir then
    return false
  end
  if a.symlink and a.symlink.is_dir then
    return false
  end
  return a.permissions.user_execute
    or a.permissions.group_execute
    or a.permissions.others_execute
end

return M
