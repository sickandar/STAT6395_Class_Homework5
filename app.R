library(shiny)
library(shinychat)
library(ellmer)
library(bslib)
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(leaflet)
library(bsicons)

zip_locations <- read_csv("zip_locations.csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(zip = str_pad(as.character(.data$zip), width = 5, side = "left", pad = "0"))

zip_hardiness <- read_csv("zip_hardiness.csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(zip = str_pad(as.character(.data$zip), width = 5, side = "left", pad = "0"))

zip_data <- zip_locations |>
  left_join(zip_hardiness, by = "zip")

plant_care_lookup <- function(plant) {
  plant <- tolower(trimws(plant))
  
  care_db <- list(
    tomato = paste(
      "Tomatoes prefer full sun, warm weather, and evenly moist soil.",
      "Water deeply at the base several times per week depending on heat.",
      "Use a cage or stake for support.",
      "Yellow lower leaves often suggest watering inconsistency or drainage problems."
    ),
    basil = paste(
      "Basil prefers full sun and consistently moist but not soggy soil.",
      "Pinch flower buds to encourage leaf growth.",
      "Protect it from cold temperatures."
    ),
    pothos = paste(
      "Pothos grows best in bright indirect light.",
      "Water when the top inch of soil is dry.",
      "Yellow leaves often suggest overwatering."
    ),
    "snake plant" = paste(
      "Snake plants prefer bright indirect light but tolerate lower light.",
      "Let the soil dry thoroughly between waterings.",
      "Overwatering is the most common problem."
    ),
    rose = paste(
      "Roses prefer full sun, good airflow, and deep watering at the base.",
      "Prune dead or weak growth regularly.",
      "Watch for fungal problems such as black spot."
    ),
    lavender = paste(
      "Lavender prefers full sun and sharply drained soil.",
      "Do not overwater.",
      "It performs best in drier conditions than many garden plants."
    )
  )
  
  if (plant %in% names(care_db)) return(care_db[[plant]])
  "I do not have a saved profile for that plant. Provide general advice based on sunlight, drainage, watering frequency, and season."
}

diagnose_plant_problem <- function(symptom) {
  symptom <- tolower(trimws(symptom))
  if (grepl("yellow", symptom)) return("Possible causes include overwatering, poor drainage, nutrient deficiency, or normal aging of older leaves.")
  if (grepl("brown", symptom)) return("Possible causes include underwatering, low humidity, salt buildup, sun scorch, or damaged roots.")
  if (grepl("wilting", symptom)) return("Possible causes include underwatering, heat stress, transplant shock, root damage, or severe overwatering.")
  if (grepl("spots|spot", symptom)) return("Possible causes include fungal disease, bacterial disease, pest damage, or leaf burn from water and sun exposure.")
  "Common causes include watering problems, light mismatch, drainage issues, pests, or disease."
}

care_tool <- tool(
  plant_care_lookup,
  description = "Look up basic care advice for a specific plant.",
  arguments = list(
    plant = type_string("The plant name, such as tomato, basil, pothos, snake plant, rose, or lavender.")
  )
)

diagnosis_tool <- tool(
  diagnose_plant_problem,
  description = "Suggest likely causes for a plant symptom.",
  arguments = list(
    symptom = type_string("A short symptom description such as yellow leaves, brown tips, wilting, or leaf spots.")
  )
)

garden_theme <- bs_theme(
  version = 5,
  bg = "#F6FBF1",
  fg = "#203020",
  primary = "#4E7A3E",
  secondary = "#A3B18A",
  success = "#588157",
  info = "#7BAE7F"
)

ui <- page_fillable(
  theme = garden_theme,
  title = "Gardening Dashboard",
  padding = "1rem",
  layout_columns(
    col_widths = c(8, 4),
    navset_card_tab(
      nav_panel(
        "Dashboard",
        layout_columns(
          col_widths = c(12),
          card(
            card_header("Garden Planning & Square Footage"),
            uiOutput("planning_card")
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Climate & Winter Survival"),
              uiOutput("climate_card")
            ),
            card(
              card_header("Companion Planting"),
              uiOutput("companion_card")
            )
          )
        )
      ),
      nav_panel(
        "Interactive Map",
        layout_columns(
          col_widths = c(12),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Current Location",
              value = textOutput("ui_city"),
              showcase = bs_icon("geo-alt"),
              theme = "primary"
            ),
            value_box(
              title = "USDA Hardiness",
              value = textOutput("ui_zone"),
              showcase = bs_icon("thermometer-sun"),
              theme = "danger"
            ),
            value_box(
              title = "Plant Capacity",
              value = textOutput("ui_capacity"),
              p(textOutput("ui_plant")),
              showcase = bs_icon("flower1"),
              theme = "success"
            )
          ),
          card(
            card_header("Garden Map"),
            leafletOutput("garden_map", height = "450px")
          )
        )
      )
    ),
    card(
      card_header("Gardening Assistant"),
      chat_ui("garden_chat")
    )
  )
)

server <- function(input, output, session) {
  
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  
  rv <- reactiveValues(
    city = "Unknown",
    zone = "-",
    capacity = 0,
    plant = "None",
    lat = 39.8283,
    lng = -98.5795,
    zoom = 3,
    sqft = NULL,
    climate_plant = "None",
    ideal_temp = NULL,
    sunlight = NULL,
    winter_result = NULL,
    companion_input = NULL,
    companion_conflicts = NULL,
    companion_recommendations = NULL,
    companion_result = NULL,
    ai_planning = NULL,
    ai_climate = NULL,
    ai_companion = NULL
  )
  
  prompt_times <- reactiveVal(as.POSIXct(character()))
  
  lookup_location <- function(location_input) {
    x <- tolower(trimws(location_input))
    digits_only <- gsub("[^0-9]", "", x)
    
    cleaned_city <- x
    cleaned_city <- gsub(",.*$", "", cleaned_city)
    cleaned_city <- gsub("\\b(i live in|i am in|i'm in|in)\\b", "", cleaned_city)
    cleaned_city <- trimws(cleaned_city)
    
    row <- if (nchar(digits_only) == 5) {
      zip_data |> filter(zip == digits_only) |> slice(1)
    } else {
      zip_data |>
        filter(str_to_lower(city) == cleaned_city) |>
        slice(1)
    }
    
    if (nrow(row) == 0) return("Location not found. Ask for a 5-digit ZIP code.")
    
    rv$city <- tools::toTitleCase(row$city[[1]])
    rv$lat <- row$latitude[[1]]
    rv$lng <- row$longitude[[1]]
    rv$zoom <- 10
    
    if ("usda_zone" %in% names(row) && !is.na(row$usda_zone[[1]])) {
      rv$zone <- paste("Zone", row$usda_zone[[1]])
    }
    
    paste0(
      "ZIP: ", row$zip[[1]],
      "; City: ", row$city[[1]],
      "; State: ", row$state_id[[1]],
      "; Latitude: ", round(row$latitude[[1]], 4),
      "; Longitude: ", round(row$longitude[[1]], 4),
      if ("usda_zone" %in% names(row) && !is.na(row$usda_zone[[1]])) paste0("; USDA Zone: ", row$usda_zone[[1]]) else ""
    )
  }
  
  check_winter_survival <- function(plant, location_input = "") {
    plant <- tolower(trimws(plant))
    location_input <- trimws(location_input)
    digits_only <- gsub("[^0-9]", "", location_input)
    
    row <- if (nchar(digits_only) == 5) {
      zip_data |> filter(zip == digits_only) |> slice(1)
    } else if (nzchar(location_input)) {
      cleaned_city <- tolower(location_input)
      cleaned_city <- gsub(",.*$", "", cleaned_city)
      cleaned_city <- gsub("\\b(i live in|i am in|i'm in|in)\\b", "", cleaned_city)
      cleaned_city <- trimws(cleaned_city)
      
      zip_data |> filter(str_to_lower(city) == cleaned_city) |> slice(1)
    } else {
      zip_data[0, ]
    }
    
    climate_db <- list(
      tomato = list(
        type = "annual",
        min_zone = NA,
        ideal_temp = "70 to 85 F",
        sunlight = "Full sun",
        note = "Tomatoes usually do not survive winter outdoors."
      ),
      basil = list(
        type = "annual",
        min_zone = NA,
        ideal_temp = "70 to 90 F",
        sunlight = "Full sun",
        note = "Basil dies after frost."
      ),
      pothos = list(
        type = "houseplant",
        min_zone = NA,
        ideal_temp = "65 to 85 F",
        sunlight = "Bright indirect light",
        note = "Pothos should be kept indoors."
      ),
      "snake plant" = list(
        type = "houseplant",
        min_zone = NA,
        ideal_temp = "60 to 85 F",
        sunlight = "Bright indirect to low light",
        note = "Keep indoors in winter."
      ),
      rose = list(
        type = "perennial",
        min_zone = 4,
        ideal_temp = "60 to 75 F",
        sunlight = "Full sun",
        note = "Many roses can overwinter outdoors."
      ),
      lavender = list(
        type = "perennial",
        min_zone = 5,
        ideal_temp = "60 to 80 F",
        sunlight = "Full sun",
        note = "Survives winter if drainage is excellent."
      )
    )
    
    if (!(plant %in% names(climate_db))) {
      return("Plant not in database. Tell the user it is not in the database.")
    }
    
    plant_info <- climate_db[[plant]]
    
    rv$climate_plant <- tools::toTitleCase(plant)
    rv$ideal_temp <- plant_info$ideal_temp
    rv$sunlight <- plant_info$sunlight
    
    if (nrow(row) == 0) {
      rv$winter_result <- if (nzchar(location_input)) "Location not found." else "Location not provided."
      
      return(
        paste(
          "Plant:", tools::toTitleCase(plant), "|",
          "Ideal temperature:", plant_info$ideal_temp, "|",
          "Sunlight:", plant_info$sunlight, "|",
          "Winter survival:", rv$winter_result
        )
      )
    }
    
    rv$city <- tools::toTitleCase(row$city[[1]])
    rv$lat <- row$latitude[[1]]
    rv$lng <- row$longitude[[1]]
    rv$zoom <- 10
    
    if ("usda_zone" %in% names(row) && !is.na(row$usda_zone[[1]])) {
      rv$zone <- paste("Zone", row$usda_zone[[1]])
    }
    
    zone <- if ("usda_zone" %in% names(row)) row$usda_zone[[1]] else NA
    winter_text <- NULL
    
    if (is.na(zone) || zone == "") {
      winter_text <- plant_info$note
    } else if (plant_info$type %in% c("annual", "houseplant")) {
      winter_text <- plant_info$note
    } else {
      zone_num <- suppressWarnings(as.numeric(gsub("[abAB]", "", zone)))
      if (!is.na(zone_num) && zone_num >= plant_info$min_zone) {
        winter_text <- paste(
          tools::toTitleCase(plant),
          "is likely to survive winter in zone", zone, ".",
          plant_info$note
        )
      } else {
        winter_text <- paste(
          tools::toTitleCase(plant),
          "may not survive winter in zone", zone,
          "and usually needs zone", plant_info$min_zone, "or warmer.",
          plant_info$note
        )
      }
    }
    
    rv$winter_result <- winter_text
    
    paste(
      "Plant:", tools::toTitleCase(plant), "|",
      "Location:", row$city[[1]], "|",
      "USDA zone:", zone, "|",
      "Ideal temperature:", plant_info$ideal_temp, "|",
      "Sunlight:", plant_info$sunlight, "|",
      "Winter survival:", winter_text
    )
  }
  
  calculate_spacing <- function(square_footage, plant) {
    sqft <- as.numeric(gsub("[^0-9.]", "", square_footage))
    plant <- tolower(trimws(plant))
    
    spacing_db <- list(
      tomato = 4,
      basil = 1,
      rose = 9,
      lavender = 4,
      cabbage = 2,
      potato = 2,
      carrot = 0.25,
      onion = 0.25
    )
    
    if (!(plant %in% names(spacing_db))) return("I do not have exact spacing data. Provide a general estimate.")
    
    count <- floor(sqft / spacing_db[[plant]])
    
    rv$capacity <- count
    rv$plant <- tools::toTitleCase(plant)
    rv$sqft <- sqft
    
    paste("Based on", sqft, "square feet, there is room for approximately", count, plant, "plants.")
  }
  
  analyze_companions <- function(plant_list) {
    companion_data <- list(
      tomato = list(good = c("basil", "marigold", "carrot", "onion"), bad = c("potato", "cabbage", "fennel")),
      potato = list(good = c("beans", "corn", "cabbage", "pea"), bad = c("tomato", "squash", "cucumber")),
      basil = list(good = c("tomato", "pepper", "oregano"), bad = c("rue")),
      carrot = list(good = c("tomato", "lettuce", "radish", "onion"), bad = c("dill", "parsnip")),
      cabbage = list(good = c("potato", "celery", "dill", "onion"), bad = c("tomato", "strawberry", "beans")),
      marigold = list(good = c("tomato", "pepper", "cucumber", "squash"), bad = c()),
      onion = list(good = c("tomato", "cabbage", "carrot", "pepper"), bad = c("pea", "beans"))
    )
    
    plants <- tolower(trimws(unlist(plant_list)))
    known_plants <- plants[plants %in% names(companion_data)]
    
    rv$companion_input <- known_plants
    rv$companion_conflicts <- character(0)
    rv$companion_recommendations <- character(0)
    
    if (length(known_plants) == 0) {
      rv$companion_result <- "None of the provided plants are in the database."
      return("None of the provided plants are in the database. Provide general advice.")
    }
    
    conflicts <- c()
    recommendations <- c()
    
    if (length(known_plants) > 1) {
      for (i in 1:(length(known_plants) - 1)) {
        for (j in (i + 1):length(known_plants)) {
          p1 <- known_plants[i]
          p2 <- known_plants[j]
          
          if (p2 %in% companion_data[[p1]]$bad || p1 %in% companion_data[[p2]]$bad) {
            conflicts <- c(conflicts, paste(tools::toTitleCase(p1), "and", tools::toTitleCase(p2)))
          }
        }
      }
    }
    
    for (p in known_plants) {
      recommendations <- c(recommendations, companion_data[[p]]$good)
    }
    
    recommendations <- unique(recommendations)
    recommendations <- recommendations[!(recommendations %in% known_plants)]
    
    rv$companion_conflicts <- conflicts
    rv$companion_recommendations <- recommendations
    rv$companion_result <- paste(
      "Companion analysis for", paste(known_plants, collapse = ", "),
      if (length(conflicts) > 0) paste("| Conflicts:", paste(conflicts, collapse = ", ")) else "| No known conflicts.",
      if (length(recommendations) > 0) paste("| Good additions:", paste(recommendations, collapse = ", ")) else "| No suggestions."
    )
    
    paste(
      "Companion analysis for", paste(known_plants, collapse = ", "),
      "---",
      if (length(conflicts) > 0) paste("WARNING CONFLICTS:", paste(conflicts, collapse = ", ")) else "No known conflicts.",
      "---",
      if (length(recommendations) > 0) paste("GOOD ADDITIONS:", paste(recommendations, collapse = ", ")) else "None."
    )
  }
  
  location_tool <- tool(
    lookup_location,
    description = "Look up a user's ZIP code or city and return city, coordinates, and USDA zone.",
    arguments = list(
      location_input = type_string("A 5-digit ZIP code or city name.")
    )
  )
  
  winter_tool <- tool(
    check_winter_survival,
    description = "Return a plant's ideal temperature, sunlight needs, and winter survival for a given location when provided.",
    arguments = list(
      plant = type_string("Plant name."),
      location_input = type_string("A 5-digit ZIP code or city name. Use a location for winter survival questions. Leave blank for general temperature and sunlight questions.")
    )
  )
  
  spacing_tool <- tool(
    calculate_spacing,
    description = "Calculate how many plants fit in a given square footage.",
    arguments = list(
      square_footage = type_string("Total area in square feet."),
      plant = type_string("Plant name.")
    )
  )
  
  companion_tool <- tool(
    analyze_companions,
    description = "Analyze a list of plants, identify conflicts, and suggest companion plants to add.",
    arguments = list(
      plant_list = type_array(items = type_string(), description = "Array of plant names to grow together.")
    )
  )
  
  output$ui_city <- renderText({ rv$city })
  output$ui_zone <- renderText({ rv$zone })
  output$ui_capacity <- renderText({ rv$capacity })
  output$ui_plant <- renderText({
    if (rv$plant == "None") "No plant selected" else paste("Target:", rv$plant)
  })
  
  output$garden_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = rv$lng, lat = rv$lat, zoom = rv$zoom) |>
      addMarkers(lng = rv$lng, lat = rv$lat)
  })
  
  observe({
    leafletProxy("garden_map") |>
      clearMarkers() |>
      setView(lng = rv$lng, lat = rv$lat, zoom = rv$zoom) |>
      addMarkers(lng = rv$lng, lat = rv$lat)
  })
  
  chat <- chat_anthropic(
    model = "claude-haiku-4-5",
    params = list(max_tokens = 220),
    system_prompt = paste(
      "You are an experienced gardener helping home gardeners and beginners.",
      "Keep answers under 120 words and be extremely brief.",
      "If the user provides a ZIP code or city, use the lookup_location tool.",
      "If the user asks what can grow in their area, what zone they are in, or anything location-specific, use the lookup_location tool first.",
      "If a user asks for ideal temperature or sunlight for a plant, use the check_winter_survival tool.",
      "If a user asks about winter survival, frost, or surviving outdoors and has not provided a location:",
      "DO NOT answer yet.",
      "FIRST ask: 'What ZIP code or city are you growing this in?'",
      "If a winter-survival question includes a location, use the lookup_location tool and the check_winter_survival tool.",
      "Use the plant_care_lookup tool when the user asks about a specific plant.",
      "Use the diagnose_plant_problem tool when the user describes symptoms.",
      "Use the companion_tool when a user asks what plants grow well together, asks for companion plant suggestions, or asks what companion plants to add.",
      "Use the spacing_tool when a user asks about square footage or how many plants will fit.",
      "Use the plant database. If a plant not in the database is asked, tell the user it is not in the database.",
      "Botany topics are allowed. Other topics are NOT allowed."
    )
  )
  
  chat$register_tool(care_tool)
  chat$register_tool(diagnosis_tool)
  chat$register_tool(companion_tool)
  chat$register_tool(location_tool)
  chat$register_tool(winter_tool)
  chat$register_tool(spacing_tool)
  
  build_dashboard_insights <- function(user_question, state) {
    if (is.null(user_question) || !nzchar(user_question)) return()
    
    dashboard_chat <- chat_anthropic(
      model = "claude-haiku-4-5",
      params = list(max_tokens = 220),
      system_prompt = paste(
        "You write short dashboard summaries for a gardening app.",
        "Return exactly 3 sections and nothing else.",
        "Format exactly like this:",
        "PLANNING:",
        "- bullet 1",
        "- bullet 2",
        "CLIMATE:",
        "- bullet 1",
        "- bullet 2",
        "COMPANION:",
        "- bullet 1",
        "- bullet 2",
        "Use short bullet points only.",
        "Each section should have 2 to 3 bullets.",
        "Each bullet should be brief and specific to the user's question and provided app state.",
        "If a section is less relevant, still provide helpful bullets related to the conversation."
      )
    )
    
    context <- paste(
      "User question:", user_question,
      "\nCity:", state$city,
      "\nZone:", state$zone,
      "\nSquare feet:", state$sqft,
      "\nPlant capacity:", state$capacity,
      "\nSelected plant:", state$plant,
      "\nClimate plant:", state$climate_plant,
      "\nIdeal temp:", state$ideal_temp,
      "\nSunlight:", state$sunlight,
      "\nWinter result:", state$winter_result,
      "\nCompanion plants entered:", state$companion_input,
      "\nCompanion conflicts:", state$companion_conflicts,
      "\nCompanion suggestions:", state$companion_recommendations
    )
    
    out <- tryCatch(
      dashboard_chat$chat(context),
      error = function(e) NULL
    )
    
    if (is.null(out) || !nzchar(out)) return()
    
    lines <- trimws(strsplit(out, "\n")[[1]])
    
    extract_section_bullets <- function(lines, section_name) {
      start_idx <- which(lines == paste0(section_name, ":"))
      if (length(start_idx) == 0) return(NULL)
      
      next_headers <- which(lines %in% c("PLANNING:", "CLIMATE:", "COMPANION:"))
      next_headers <- next_headers[next_headers > start_idx[1]]
      
      end_idx <- if (length(next_headers) == 0) length(lines) else next_headers[1] - 1
      
      section_lines <- lines[(start_idx[1] + 1):end_idx]
      section_lines <- section_lines[grepl("^-\\s+", section_lines)]
      
      if (length(section_lines) == 0) return(NULL)
      
      sub("^-\\s+", "", section_lines)
    }
    
    rv$ai_planning <- extract_section_bullets(lines, "PLANNING")
    rv$ai_climate <- extract_section_bullets(lines, "CLIMATE")
    rv$ai_companion <- extract_section_bullets(lines, "COMPANION")
  }
  
  output$planning_card <- renderUI({
    if (is.null(rv$ai_planning) || length(rv$ai_planning) == 0) {
      tags$ul(tags$li("Ask a gardening question to generate planning insights."))
    } else {
      tags$ul(lapply(rv$ai_planning, tags$li))
    }
  })
  
  output$climate_card <- renderUI({
    if (is.null(rv$ai_climate) || length(rv$ai_climate) == 0) {
      tags$ul(tags$li("Ask a gardening question to generate climate insights."))
    } else {
      tags$ul(lapply(rv$ai_climate, tags$li))
    }
  })
  
  output$companion_card <- renderUI({
    if (is.null(rv$ai_companion) || length(rv$ai_companion) == 0) {
      tags$ul(tags$li("Ask a gardening question to generate companion planting insights."))
    } else {
      tags$ul(lapply(rv$ai_companion, tags$li))
    }
  })
  
  observeEvent(input$garden_chat_user_input, {
    req(input$garden_chat_user_input)
    
    now <- Sys.time()
    recent_times <- prompt_times()
    recent_times <- recent_times[difftime(now, recent_times, units = "secs") <= 60]
    
    if (length(recent_times) >= 8) {
      chat_append(
        "garden_chat",
        list(
          role = "assistant",
          content = "**Rate limit exceeded:** Please wait a minute before sending more messages."
        )
      )
      return()
    }
    
    prompt_times(c(recent_times, now))
    
    if (nchar(input$garden_chat_user_input) > 300) {
      chat_append(
        "garden_chat",
        list(
          role = "assistant",
          content = "**Input too long:** Please keep your gardening questions under 300 characters to conserve tokens."
        )
      )
      return()
    }
    
    user_msg <- input$garden_chat_user_input
    
    zip_match <- str_extract(user_msg, "\\b\\d{5}\\b")
    if (!is.na(zip_match)) {
      lookup_location(zip_match)
    }
    
    stream <- chat$stream_async(
      user_msg,
      stream = "content"
    )
    
    chat_append("garden_chat", stream)
    
    state_snapshot <- list(
      city = rv$city %||% "Unknown",
      zone = rv$zone %||% "-",
      sqft = if (is.null(rv$sqft)) "None" else as.character(rv$sqft),
      capacity = rv$capacity %||% 0,
      plant = rv$plant %||% "None",
      climate_plant = rv$climate_plant %||% "None",
      ideal_temp = if (is.null(rv$ideal_temp)) "Unknown" else rv$ideal_temp,
      sunlight = if (is.null(rv$sunlight)) "Unknown" else rv$sunlight,
      winter_result = if (is.null(rv$winter_result)) "Not checked" else rv$winter_result,
      companion_input = if (is.null(rv$companion_input) || length(rv$companion_input) == 0) {
        "None"
      } else {
        paste(rv$companion_input, collapse = ", ")
      },
      companion_conflicts = if (is.null(rv$companion_conflicts) || length(rv$companion_conflicts) == 0) {
        "None"
      } else {
        paste(rv$companion_conflicts, collapse = ", ")
      },
      companion_recommendations = if (is.null(rv$companion_recommendations) || length(rv$companion_recommendations) == 0) {
        "None"
      } else {
        paste(rv$companion_recommendations, collapse = ", ")
      }
    )
    
    later::later(function() {
      build_dashboard_insights(user_msg, state_snapshot)
    }, delay = 1.5)
  })
}

shinyApp(ui, server)