require("relative-motions"):setup({ show_numbers = "relative_absolute", show_motion = true })
require("bookmarks"):setup({
  persist = "vim",
  -- save_last_directory = true,
  desc_format = "full",
  file_pick_mode = "hover",
  last_directory = { enable = false, persist = false },
  notify = { enable = true },
})

require("starship"):setup()

require("session"):setup {
  sync_yanked = true,
}
