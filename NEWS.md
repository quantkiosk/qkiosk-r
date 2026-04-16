### Changes in 0.2.29 (2026-04-16)

- version bump
- new `yyyymmdd` features
- `qk_df` is now broken into classes to better support custom columns etc.
- `qk_account` and `qk_download` updates to utilize custom datasets
- improved checking for correct qkid objects and formats
- general improvements to data functions and api updates
- `to_cik` and `qk_cik` return valid QKIDs even if CIK is not in entity master. This allows for access outside of the QKID symbology for smaller companies or filers (people)
- table styling now handles floating point precision with better default behavior  
