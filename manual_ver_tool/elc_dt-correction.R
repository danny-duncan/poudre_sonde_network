all_data_dir <- "~/github/poudre_sonde_network/manual_verification_tool/data/all_data_directory"

elc_files <- list.files(all_data_dir, pattern = "elc", full.names = TRUE)

elc_params <- str_extract(basename(elc_files), "(?<=elc-)[^_]+")

for (i in seq_along(elc_files)) {
  df_name <- paste0("elc_", elc_params[i])
  assign(df_name, read_parquet(elc_files[i]))
}

elc_DO <- elc_DO %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
      )
    )

elc_ORP <- elc_ORP %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

elc_pH <- elc_pH %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

`elc_Specific Conductivity` <- `elc_Specific Conductivity` %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

elc_Temperature <- elc_Temperature %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

elc_Turbidity <- elc_Turbidity %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

elc_Depth <- elc_Depth %>%
  mutate(
    DT_round = if_else(
      DT_round < as.POSIXct("2022-05-27"),
      DT_round - hours(6),
      DT_round
    )
  )

write_parquet(elc_Depth, here(all_data_dir, "elc-Depth_20260505_160307_de6a9213102d7b3c6b4928121d2a03b9.parquet"))
write_parquet(elc_DO, here(all_data_dir, "elc-DO_20260505_160307_ff2d68f4ac35383c8fb59a479174dbc5.parquet"))
write_parquet(elc_ORP, here(all_data_dir, "elc-ORP_20260505_160307_1e2eab1a87ee90db340b3bef61168df3.parquet"))
write_parquet(elc_pH, here(all_data_dir, "elc-pH_20260505_160307_bf9e07c50919e854619c47d86750bab4.parquet"))
write_parquet(`elc_Specific Conductivity`, here(all_data_dir, "elc-Specific Conductivity_20260505_160307_d1b4ef2ef58d7149105db005c90b11b1.parquet"))
write_parquet(elc_Temperature, here(all_data_dir, "elc-Temperature_20260505_160307_bfc8f23b0b1afb9268e8094508e5e937.parquet"))
write_parquet(elc_Turbidity, here(all_data_dir, "elc-Turbidity_20260505_160307_6509c4e8a0e6ef643f2b4834f6599c27.parquet"))
