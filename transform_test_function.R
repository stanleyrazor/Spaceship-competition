

transform_data <- function(data)
{
  # splitting passenger ID
  odd_id <- seq(from = 1, to = nrow(data)*2, by = 2)
  id_loc <- function(vec, id) vec[id] # function for vector location '[.]'
  rep_unclassified <- function(vec) ifelse(vec == '', 'Unclassified', vec)   # fill '' wih Unclassfied

  data <- data |>
    mutate(Cabin = ifelse(test = Cabin == '', yes = '//', no = Cabin)) |>
    mutate(Group = (str_split(PassengerId, '_') |> unlist())[odd_id],
           PID = (str_split(PassengerId, '_') |> unlist())[-odd_id],
           CabinDeck = lapply((str_split(Cabin, '/')), id_loc, 1) |> unlist(),
           CabinNum = lapply((str_split(Cabin, '/')), id_loc, 2) |> unlist(),
           CabinSide = lapply((str_split(Cabin, '/')), id_loc, 3) |> unlist()) |>
    select(PID, Group, HomePlanet, CryoSleep, CabinDeck, CabinNum, CabinSide,
           Destination, Age, VIP, RoomService,
           FoodCourt, ShoppingMall, Spa, VRDeck)
  
  # imputting VRDeck
  newVR <- data |>
    aggregate(VRDeck ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
    setNames(c('HomePlanet', 'CryoSleep', 'newVR'))
  
  # imputing room service
  newRoomService <- data |>
    aggregate(RoomService ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
    setNames(c('HomePlanet', 'CryoSleep', 'newRoomService'))
  
  # imputting food court
  newFoodCourt <- data |>
    aggregate(FoodCourt ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
    setNames(c('HomePlanet', 'CryoSleep', 'newFoodCourt'))
  
  # imputting shopping mall
  newShoppingMall <- data |>
    aggregate(ShoppingMall ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
    setNames(c('HomePlanet', 'CryoSleep', 'newShoppingMall'))
  
  # imputting spa
  newSpa <- data |>
    aggregate(Spa ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
    setNames(c('HomePlanet', 'CryoSleep', 'newSpa'))
  
  # Age estimatred using deck and cabin side
  newAge <- data |>
    aggregate(Age ~ PID + HomePlanet, mean, na.omit = T) |>
    setNames(c('PID', 'HomePlanet', 'newAge'))
  
  data <- data |>
    merge(newVR,
          by = c('HomePlanet', 'CryoSleep'),
          all.x = T) |>
    merge(newRoomService,
          by = c('HomePlanet', 'CryoSleep'),
          all.x = T) |>
    merge(newFoodCourt,
          by = c('HomePlanet', 'CryoSleep'),
          all.x = T) |>
    merge(newShoppingMall,
          by = c('HomePlanet', 'CryoSleep'),
          all.x = T)|>
    merge(newSpa,
          by = c('HomePlanet', 'CryoSleep'),
          all.x = T) |>
    merge(newAge,
          by = c('PID', 'HomePlanet'),
          all.x = T)
  
  
  data <- data |>
    mutate(
      RoomService = ifelse(is.na(RoomService), newRoomService, RoomService),
      FoodCourt = ifelse(is.na(FoodCourt), newFoodCourt, FoodCourt),
      ShoppingMall = ifelse(is.na(ShoppingMall), newShoppingMall, ShoppingMall),
      Spa = ifelse(is.na(Spa), newSpa, Spa),
      VRDeck = ifelse(is.na(VRDeck), newVR, VRDeck),
      Age = ifelse(is.na(Age), newAge, Age)
    ) |>
    select(c("Group", "PID", "VIP", "HomePlanet", "CryoSleep", "CabinDeck", 
             "CabinSide", "Destination", "Age", "RoomService", 
             "FoodCourt", "ShoppingMall", "Spa", "VRDeck"))
  
  
  data <- data |>
    mutate(
      across(
        .cols = where(is.character),
        .fns = rep_unclassified
      )
    ) |>
    mutate(
      across(
        .cols = where(is.character),
        .fns = as.factor
      )
    )
  
  
  return(data)
}


# xx <- transform_data(test)
