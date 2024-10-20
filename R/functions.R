# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Cleans a dataframe, removing '*', replacing spaces with '_' and changing all text to lower case.
#'
#' @param df A dataframe.
#' @returns A dataframe.
#' @examples
#' lower_clean_dataframe(df)
lowercase_clean_dataframe <- function(df) {
  colnames(df) <- tolower(gsub("\\*", "", gsub(" ", "_", colnames(df))))
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      tolower(gsub("\\*", "", gsub(" ", "_", x)))
    } else {
      x
    }
  })
  return(df)
}




#' Changes a StatsBomb events dataframe, condensing the information in columns into the "type_name" column.
#'
#' @param df A StatsBomb event dataframe, returned from free_allevents(), in the 'StatsBombR' package.
#' @returns A dataframe.
#' @examples
#' modify_statsbomb_events_dataframe(statsbomb_event_df)
modify_statsbomb_events_dataframe <- function(df) {
  df[["event.end_location"]] <- NA
  df[["other_player.name"]] <- NA

  pass_condition <- df[["type.name"]] %in% "pass"
  df[["type.name"]][pass_condition] <- paste0(
    ifelse(is.na(df[["pass.outcome.name"]][pass_condition]), "complete", df[["pass.outcome.name"]][pass_condition]),
    "_",
    ifelse(is.na(df[["pass.technique.name"]][pass_condition]), "", paste0(df[["pass.technique.name"]][pass_condition], "_")),
    df[["pass.height.name"]][pass_condition],
    "_from_",
    ifelse(is.na(df[["pass.body_part.name"]][pass_condition]), ifelse(is.na(df[["pass.type.name"]][pass_condition]), "aerial_won", df[["pass.type.name"]][pass_condition]), df[["pass.body_part.name"]][pass_condition])
  )

  df[["event.end_location"]][pass_condition] <- df[["pass.end_location"]][pass_condition]
  df[["other_player.name"]][pass_condition] <- df[["pass.recipient.name"]][pass_condition]

  ball_receipt_condition <- df[["type.name"]] %in% "ball_receipt"
  df[["type.name"]][ball_receipt_condition] <- paste0(ifelse(is.na(df[["ball_receipt.outcome.name"]][ball_receipt_condition]), "", paste0(df[["ball_receipt.outcome.name"]][ball_receipt_condition], "_")), "ball_receipt")

  dribble_condition <- df[["type.name"]] %in% "dribble"
  df[["type.name"]][dribble_condition] <- paste0(df[["dribble.outcome.name"]][dribble_condition], "_dribble")

  interception_condition <- df[["type.name"]] %in% "interception"
  df[["type.name"]][interception_condition] <- paste0(df[["interception.outcome.name"]][interception_condition], "_interception")


  duel_condition <- df[["type.name"]] %in% "duel"
  df[["type.name"]][duel_condition] <- paste0(ifelse(is.na(df[["duel.outcome.name"]][duel_condition]), "", paste0(df[["duel.outcome.name"]][duel_condition], "_")), df[["duel.type.name"]][duel_condition])

  clearance_condition <- df[["type.name"]] %in% "clearance"
  df[["type.name"]][clearance_condition] <- paste0("clearance_from_", df[["clearance.body_part.name"]][clearance_condition])

  foul_committed_condition <- df[["type.name"]] %in% "foul_committed"
  df[["type.name"]][foul_committed_condition] <- paste0(
    ifelse(is.na(df[["foul_committed.offensive"]][foul_committed_condition]), "", "offensive_"),
    ifelse(is.na(df[["foul_committed.card.name"]][foul_committed_condition]), "", paste0(df[["foul_committed.card.name"]][foul_committed_condition], "_")),
    "foul_committed",
    ifelse(is.na(df[["foul_committted.type.name"]][foul_committed_condition]), "", paste0("_", df[["foul_committted.type.name"]][foul_committed_condition], "_")),
    ifelse(is.na(df[["foul_committed.advantage"]][foul_committed_condition]), "", "_advantage_played")
  )

  foul_won_condition <- df[["type.name"]] %in% "foul_won"
  df[["type.name"]][foul_won_condition] <- paste0(
    ifelse(is.na(df[["foul_won.defensive"]][foul_won_condition]), "", "defensive_"),
    "foul_won",
    ifelse(is.na(df[["foul_won.advantage"]][foul_won_condition]), "", "_advantage_played")
  )

  shot_condition <- df[["type.name"]] %in% "shot"
  df[["type.name"]][shot_condition] <- paste0(
    ifelse(is.na(df[["shot.one_on_one"]][shot_condition]), "", "one_on_one_"),
    ifelse(is.na(df[["shot.first_time"]][shot_condition]), "", "first_time_"),
    ifelse(is.na(df[["shot.aerial_won"]][shot_condition]), "", "aerial_won_"),
    ifelse(is.na(df[["shot.deflected"]][shot_condition]), "", "deflected_"),
    df[["shot.technique.name"]][shot_condition],
    "_shot_with_",
    df[["shot.body_part.name"]][shot_condition],
    "_from_",
    df[["shot.type.name"]][shot_condition],
    "_which_was_",
    df[["shot.outcome.name"]][shot_condition]
  )

  df[["event.end_location"]][shot_condition] <- df[["shot.end_location"]][shot_condition]
  df[["other_player.name"]][shot_condition] <- df[["player.name.gk"]][shot_condition]

  block_condition <- df[["type.name"]] %in% "block"
  df[["type.name"]][block_condition] <- paste0(
    ifelse(is.na(df[["block.offensive"]][block_condition]), "", "offensive_"),
    ifelse(is.na(df[["block.save_block"]][block_condition]), "", "save_"),
    ifelse(is.na(df[["block.deflection"]][block_condition]), "", "deflected_"),
    "block"
  )

  fifty_fifty_condition <- df[["type.name"]] %in% "50/50"
  df[["type.name"]][fifty_fifty_condition] <- paste0(df[["50_50.outcome.name"]][fifty_fifty_condition], "_50_50")


  substitution_condition <- df[["type.name"]] %in% "substitution"
  df[["type.name"]][substitution_condition] <- paste0(df[["substitution.outcome.name"]][substitution_condition], "_substitution")

  df[["other_player.name"]][substitution_condition] <- df[["substitution.replacement.name"]][substitution_condition]

  bad_behaviour_condition <- df[["type.name"]] %in% "bad_behaviour"
  df[["type.name"]][bad_behaviour_condition] <- df[["bad_behaviour.card.name"]][bad_behaviour_condition]

  ball_recovery_condition <- df[["type.name"]] %in% "ball_recovery"
  df[["type.name"]][ball_recovery_condition] <- paste0(ifelse(is.na(df[["ball_recovery.recovery_failure"]][ball_recovery_condition]), "", "failed_"),
                                                       ifelse(is.na(df[["ball_recovery.offensive"]][ball_recovery_condition]), "", "offensive_"),
                                                       "ball_recovery")

  goalkeeper_condition <- df[["type.name"]] %in% "goal_keeper"
  df[["type.name"]][goalkeeper_condition] <- paste0(
    ifelse(is.na(df[["goalkeeper.position.name"]][goalkeeper_condition]), "", paste0(df[["goalkeeper.position.name"]][goalkeeper_condition], "_")),
    "goalkeeper_",
    df[["goalkeeper.type.name"]][goalkeeper_condition],
    ifelse(is.na(df[["goalkeeper.outcome.name"]][goalkeeper_condition]), "", paste0("_", df[["goalkeeper.outcome.name"]][goalkeeper_condition])),
    ifelse(is.na(df[["goalkeeper.technique.name"]][goalkeeper_condition]), "", paste0("_by_", df[["goalkeeper.technique.name"]][goalkeeper_condition])),
    ifelse(is.na(df[["goalkeeper.body_part.name"]][goalkeeper_condition]), "", paste0("_using_", df[["goalkeeper.body_part.name"]][goalkeeper_condition]))
  )

  df[["event.end_location"]][shot_condition] <- df[["goalkeeper.end_location"]][shot_condition]

  carry_condition <- df[["type.name"]] %in% "carry"

  df[["event.end_location"]][carry_condition] <- df[["carry.end_location"]][carry_condition]

  columns <- c(
    "id",
    "match_id",
    "period",
    "timestamp",
    "type.name",
    "location",
    "event.end_location",
    "play_pattern.name",
    "team_name",
    "possession_team.name",
    "player.name",
    "position.name",
    "other_player.name",
    "possession",
    "timeinposs",
    "shot.statsbomb_xg",
    "shot.freeze_frame",
    "location.x.gk",
    "location.y.gk",
    "avevelocity",
    "attackersbehindball",
    "defendersbehindball",
    "under_pressure",
    "counterpress",
    "off_camera",
    "out",
    "related_events"
  )

  df <- df[, intersect(columns, colnames(df)), drop = FALSE]


  return(df)
}
