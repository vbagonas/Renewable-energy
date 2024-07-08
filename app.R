
library(readxl)
library(dplyr)
library(leaflet)
library(shiny)
library(viridis)
library(ggplot2)
library(countrycode)
library(tidyr)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(purrr)
library(ggraph)
library(igraph)
duomenys <- read_excel("IRENA_install_gener.xlsx", sheet = "All Data")

#---------------------------------------VYTENIO DUOMENYS---------------------------------------

rusijos_duom <- read_excel("IRENA_install_gener.xlsx", sheet = "All Data")
rusijos_duom <- rusijos_duom %>%
  filter(Country == "Russian Federation (the)") %>%
  select(Region, Country, `ISO3 code`, `RE or Non-RE`, Year, `Electricity Generation (GWh)`)

europos_duomenys <- duomenys %>%
  filter(Region == "Europe") %>%
  select(Region, Country, `ISO3 code`, `RE or Non-RE`, Year, `Electricity Generation (GWh)`)

europos_duomenys <- rbind(europos_duomenys, rusijos_duom)

summarized_data <- europos_duomenys %>%
  group_by(Country, `ISO3 code`, Year) %>%
  summarize(
    Renewable = sum(`Electricity Generation (GWh)`[`RE or Non-RE` == "Total Renewable"], na.rm = TRUE),
    Non_Renewable = sum(`Electricity Generation (GWh)`[`RE or Non-RE` == "Total Non-Renewable"], na.rm = TRUE)
  ) %>%
  mutate(Renewable_part = paste0(round(Renewable / (Renewable + Non_Renewable) * 100, 2), "%")
  ) %>%
  filter(Renewable_part != "NaN%")

coords <- data.frame(
  Country = c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GRC",
              "HUN", "ISL", "IRL", "ITA", "XKX", "LVA", "LTU", "LUX", "MLT", "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "MDA", "ROU",
              "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR", "RUS"),
  Latitude = c(41.1533, 42.5063, 47.5162, 53.7098, 50.8503, 43.9159, 42.7339, 45.1, 35.1264, 49.8175, 56.2639, 58.5953, 61.8926, 61.9241, 46.6034, 51.1657, 39.0742,
               47.1625, 64.9631, 53.4129, 41.8719, 42.6026, 56.8796, 55.1694, 49.8153, 35.9375, 42.7087, 52.1326, 41.6086, 60.472, 51.9194, 39.3999, 47.4116, 45.9432,
               44.0165, 48.669, 46.1512, 40.4637, 59.3293, 46.8182, 48.3794, 55.3781, 55.7558),
  Longitude = c(20.1683, 1.5218, 14.5501, 27.9534, 4.3517, 17.6791, 25.4858, 15.2, 33.4299, 15.473, 9.5018, 25.0136, -6.9118, 25.7482, 1.8883, 10.4515, 21.8243,
                19.5033, -19.0208, -8.2439, 12.5674, 20.902, 24.6032, 23.8813, 6.1296, 14.3754, 19.3744, 5.2913, 21.7453, 8.4689, 19.1451, -8.2245, 28.3699, 24.9668,
                21.0059, 19.699, 14.9955, -3.7492, 18.064, 8.2275, 31.1656, -3.436, 37)
)
summarized_data <- summarized_data %>%
  left_join(coords, by = c("ISO3 code" = "Country")) %>%
  mutate(Renewable_part_num = as.numeric(sub("%", "", Renewable_part)))

#-----------------------------------------------------EDVINO DUOMENYS-----------------------------------------------------

Total_produc <- read_excel("Energy EU.xlsx", sheet = "Sheet 1", range = "A10:X50")%>% slice(-1) %>% rename(Country = "TIME")
Ren_produc <- read_excel("Energy EU.xlsx", sheet = "Sheet 2", range = "A10:X50") %>% slice(-1) %>% rename(Country = "TIME")
Non_produc <- read_excel("Energy EU.xlsx", sheet = "Sheet 3", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Total_consump <- read_excel("Energy EU.xlsx", sheet = "Sheet 4", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Ren_consump <- read_excel("Energy EU.xlsx", sheet = "Sheet 5", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Non_consump <- read_excel("Energy EU.xlsx", sheet = "Sheet 6", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Total_supply <- read_excel("Energy EU.xlsx", sheet = "Sheet 7", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Ren_supply <- read_excel("Energy EU.xlsx", sheet = "Sheet 8", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")
Non_supply <- read_excel("Energy EU.xlsx", sheet = "Sheet 9", range = "A10:X50")%>% slice(-1)%>% rename(Country = "TIME")

Ren_produc <- Ren_produc %>%
  mutate(across(-Country, as.character))

Ren_consump <- Ren_consump %>%
  mutate(across(-Country, as.character))

Total_produc <- Total_produc %>%
  mutate(across(-Country, as.character))

Total_supply <- Total_supply %>%
  mutate(across(-Country, as.character))

Ren_supply <- Ren_supply %>%
  mutate(across(-Country, as.character))

Ren_produc_long <- Ren_produc %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Production") %>%
  mutate(Production = ifelse(Production == ":", NA, Production)) %>%
  mutate(Production = as.numeric(Production))

Ren_consump_long <- Ren_consump %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Consumption") %>%
  mutate(Consumption = ifelse(Consumption == ":", NA, Consumption)) %>%
  mutate(Consumption = as.numeric(Consumption))

combined_data1 <- merge(Ren_produc_long, Ren_consump_long, by = c("Country", "Year")) %>%
  na.omit()
combined_data1 <- combined_data1 %>%
  mutate(Country = countrycode(Country, "country.name", "cldr.short.lt"))
percentage_data <- Total_produc %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Total_Production") %>%
  inner_join(
    Total_supply %>%
      pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Total_Supply"),
    by = c("Country", "Year")
  ) %>%
  mutate(Total_Production = ifelse(Total_Production == ":", NA, Total_Production)) %>%
  mutate(Total_Production = as.numeric(Total_Production))%>%
  mutate(Total_Supply = ifelse(Total_Supply == ":", NA, Total_Supply)) %>%
  mutate(Total_Supply = as.numeric(Total_Supply))%>%
  mutate(Production_Percentage = (Total_Production / Total_Supply) * 100)

ren_supply_long <- Ren_supply %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Ren_Supply")

combined_data2 <- percentage_data %>%
  inner_join(ren_supply_long, by = c("Country", "Year")) %>%
  filter(!is.na(Production_Percentage), !is.na(Ren_Supply))%>%
  mutate(Ren_Supply = as.numeric(Ren_Supply)) %>%
  mutate(Country = countrycode(Country, "country.name", "cldr.short.lt"))

#-----------------------------------------------------MANTO DUOMENYS-----------------------------------------------------

costs <- read_excel("costs_offgrid 1.xlsx", sheet = "costs")
col_names <- colnames(costs) 
col_names[4:16] <- paste0("X", col_names[4:16])
colnames(costs) <- col_names

#-----------------------------------------------------JURGIO DUOMENYS-----------------------------------------------------

emisijos <- read_excel("emisijos.xlsx", sheet = "Sheet3")
file_path <- "energijos_vartojimas (1).xlsx"
data <- read.xlsx(file_path, sheet = "Data", cols = c(1, 2, 113))  # Columns A (1), B (2), and DI (113)

emisijos <- emisijos %>%
  pivot_longer(cols = -`GEO (Labels)`, names_to = "year", values_to = "greenhouse_emissions_per_capita") %>%
  na.omit()

unique_countries <- unique(emisijos$`GEO (Labels)`)

europe_data <- data %>%
  filter(country %in% unique_countries, year >= 2000) %>%
  na.omit()

emisijos <- emisijos %>%
  rename(country = `GEO (Labels)`) %>%
  mutate(year = as.double(year))

joined_data_em <- left_join(emisijos, europe_data, by = c("year", "country")) %>%
  na.omit()

joined_data_em <- joined_data_em %>%
  filter(country != "United Kingdom" & country != "Iceland" & country != "Latvia") #Na and only 1 value

joined_data_em <- joined_data_em %>%
  mutate(country = countrycode(country, "country.name", "cldr.short.lt"))

#-----------------------------------------MIGLĖS DUOMENYS------------------------------------------------
filter_top_5_partners <- function(file_path, siec_mappings, country_mappings) {
  
  sff <- read.csv(file_path)
  sff <- sff %>%
    filter(!geo %in% c("EU27_2020", "NSP", "EA20", "EX_SU_OTH", "EUR_OTH", "AFR_OTH", "AME_OTH", "ASI_NME_OTH", "ASI_OTH", NA) &
             !partner %in% c("EU27_2020", "NSP", "EA20", "EX_SU_OTH", "EUR_OTH", "AFR_OTH", "AME_OTH", "ASI_NME_OTH", "ASI_OTH", NA) &
             OBS_VALUE != 0)
  
  sff$siec <- siec_mappings[sff$siec]
  
  sff$partner <- country_mappings[sff$partner]
  sff$geo <- country_mappings[sff$geo]
  
  total_exports <- sff %>%
    filter(partner == "Total") %>%
    select(TIME_PERIOD, geo, total_export = OBS_VALUE)
  
  sff <- sff %>%
    left_join(total_exports, by = c("TIME_PERIOD", "geo"))
  
  countries_to_exclude <- sff %>%
    group_by(TIME_PERIOD, geo) %>%
    summarize(other_partners = any(partner != "Total"), .groups = "drop") %>%
    filter(!other_partners) %>%
    select(TIME_PERIOD, geo)
  
  sff <- sff %>%
    anti_join(countries_to_exclude, by = c("TIME_PERIOD", "geo"))
  
  filtered_data <- sff %>%
    filter(partner == "Total") %>%
    group_by(TIME_PERIOD) %>%
    arrange(desc(OBS_VALUE)) %>%
    slice_head(n = 10) %>%
    ungroup()
  
  sff_lists <- filtered_data %>%
    split(.$TIME_PERIOD) %>%
    map(~ .x$geo)
  
  filter_by_year <- function(data, year, geo_list) {
    data %>%
      filter(TIME_PERIOD == year & geo %in% geo_list)
  }
  
  years <- names(sff_lists)
  filtered_sff <- map2_df(years, sff_lists, ~ filter_by_year(sff, as.numeric(.x), .y))
  
  filtered_sff <- filtered_sff %>% filter(partner != "Total")
  
  filtered_top_5_partners <- filtered_sff %>%
    group_by(TIME_PERIOD, geo) %>%
    arrange(desc(OBS_VALUE)) %>%
    slice_head(n = 5) %>%
    ungroup()
  
  return(filtered_top_5_partners)
}

siec_mappings <- c(
  "C0000X0350-0370" = "Solid fossil fuels","C0100" = "Hard coal","C0110" = "Anthracite","C0121" = "Coking coal","C0129" = "Other bituminous coal","C0200" = "Brown coal",
  "C0210" = "Sub-bituminous coal","C0220" = "Lignite","C0311" = "Coke oven coke","C0320" = "Patent fuel","C0330" = "Brown coal briquettes","C0340" = "Coal tar",
  "P1100" = "Peat","P1200" = "Peat products","04693" = "Paraffin waxes","04694" = "Petroleum coke","04695" = "Bitumen","04699" = "Other oil products n.e.c.","R52108" = "Blended biogasoline",
  "R52208" = "Blended biodiesels","R52308" = "Blended bio jet kerosene","04651" = "Aviation gasoline","04652" = "Motor gasoline","04652XR52108" = "Motor gasoline (excluding biofuel portion)",
  "04653" = "Gasoline-type jet fuel","04661" = "Kerosene-type jet fuel","04661XR52308" = "Kerosene-type jet fuel (excluding biofuel portion)",
  "04669" = "Other kerosene","04671" = "Gas oil and diesel oil","04671XR52208" = "Gas oil and diesel oil (excluding biofuel portion)","046711" = "Road diesel",
  "046712" = "Heating and other gasoil","04680" = "Fuel oil","04681" = "Fuel oil (low sulphur <1%)","04682" = "Fuel oil (high sulphur >=1%)","04691" = "White spirit and special boiling point industrial spirits","04692" = "Lubricants",
  "04000" = "Oil and petroleum products","04000XBIO" = "Oil and petroleum products (excluding biofuel portion)","04100_TOT_4200-4500" = "Crude oil, NGL, refinery feedstocks, additives and oxygenates and other hydrocarbons",
  "04100_TOT_4200-4500XBIO" = "Crude oil, NGL, refinery feedstocks, additives and oxygenates and other hydrocarbons (excluding biofuel portion)","04100_TOT" = "Crude oil",
  "04200" = "Natural gas liquids","04300" = "Refinery feedstocks","04400X4410" = "Additives and oxygenates (excluding biofuel portion)","04400" = "Additives and oxygenates",
  "04500" = "Other hydrocarbons","04600" = "Oil products","04600XBIO" = "Oil products (excluding biofuel portion)","04620" = "Ethane","04630" = "Liquefied petroleum gases",
  "04640" = "Naphtha","G3000" = "Natural gas","G3200" = "Liquefied natural gas","R5111" = "Wood pellets","R5210P" = "Pure biogasoline","R5210E" = "Bioethanol",
  "R5220P" = "Pure biodiesels","R5230P" = "Pure bio jet kerosene","R5290" = "Other liquid biofuels"
)
country_mappings <- c(
  "BE" = "Belgija","BG" = "Bulgarija","CZ" = "Čekija","DK" = "Danija","DE" = "Vokietija","EE" = "Estija","IE" = "Airija","EL" = "Graikija","ES" = "Ispanija","FR" = "Prancūzija","HR" = "Kroatija",
  "IT" = "Italija","CY" = "Kipras","LV" = "Latvija","LT" = "Lietuva","LU" = "Liuksemburgas","HU" = "Vengrija","MT" = "Malta","NL" = "Nyderlandai","AT" = "Austrija","PL" = "Lenkija",
  "PT" = "Portugalija","RO" = "Rumunija","SI" = "Slovėnija","SK" = "Slovakija","FI" = "Suomija","SE" = "Švedija","IS" = "Islandija","LI" = "Lichtenšteinas","NO" = "Norvegija","CH" = "Šveicarija",
  "UK" = "Jungtinė Karalystė","BA" = "Bosnija ir Hercegovina","ME" = "Juodkalnija","MD" = "Moldova","MK" = "Šiaurės Makedonija","AL" = "Albanija","RS" = "Serbija","TR" = "Turkija",
  "UA" = "Ukraina","XK" = "Kosovas","GE" = "Gruzija","AD" = "Andora","BY" = "Baltarusija","GI" = "Gibraltaras","RU" = "Rusija","AO" = "Angola","UZ" = "Uzbekistanas",
  "CN" = "Kinija","HK" = "Honkongas","JP" = "Japonija","MN" = "Mongolija","KP" = "Šiaurės Korėja","KR" = "Pietų Korėja","TW" = "Taivanas","BD" = "Bangladešas",
  "IN" = "Indija","IR" = "Iranas","NP" = "Nepalas","PK" = "Pakistanas","LK" = "Šri Lanka","BN" = "Brunėjus","KH" = "Kambodža","ID" = "Indonezija","LA" = "Laosas",
  "MY" = "Malaizija","MM" = "Mianmaras/Birma","PH" = "Filipinai","SG" = "Singapūras","TH" = "Tailandas","TL" = "Rytų Timoras","VN" = "Vietnamas","AM" = "Armėnija",
  "AZ" = "Azerbaidžanas","BH" = "Bahreinas","IQ" = "Irakas","IL" = "Izraelis","JO" = "Jordanija","KW" = "Kuveitas","LB" = "Libanas","OM" = "Omanas","QA" = "Kataras",
  "SA" = "Saudo Arabija","SY" = "Sirija","AE" = "Jungtiniai Arabų Emyratai","YE" = "Jemenas","AU" = "Australija","NZ" = "Naujoji Zelandija","NC" = "Naujoji Kaledonija",
  "PG" = "Papua Naujoji Gvinėja","AR" = "Argentina","BR" = "Brazilija","CA" = "Kanada","MH" = "Maršalo Salos","AF" = "Afganistanas","DZ" = "Alžyras","AS" = "Amerikos Samoa",
  "AO" = "Angola","AI" = "Angilija","AQ" = "Antarktida","AG" = "Antigva ir Barbuda","AR" = "Argentina","AM" = "Armėnija","AW" = "Aruba","AU" = "Australija",
  "AT" = "Austrija","AZ" = "Azerbaidžanas","BS" = "Bahamos","BH" = "Bahreinas","BD" = "Bangladešas","BB" = "Barbadosas","BY" = "Baltarusija","BE" = "Belgija",
  "BZ" = "Belizas","BJ" = "Beninas","BM" = "Bermuda","BT" = "Butanas","BO" = "Bolivija","BA" = "Bosnija ir Hercegovina","BW" = "Botsvana","BV" = "Buvė salos",
  "BR" = "Brazilija","IO" = "Britų Indijos vandenyno teritorija","BN" = "Brunėjus","BG" = "Bulgarija","BF" = "Burkina Fasas","BI" = "Burundis","KH" = "Kambodža","CM" = "Kamerūnas",
  "CA" = "Kanada","CV" = "Žaliasis Kyšulys","KY" = "Kaimanų salos","CF" = "Centrinės Afrikos Respublika","TD" = "Čadas","CL" = "Čilė","CN" = "Kinija","CX" = "Kalėdų sala","CC" = "Kokosų (Kilingo) Salos",
  "CO" = "Kolumbija","KM" = "Komorai","CG" = "Kongas","CD" = "Kongas, Demokratinė Respublika","CK" = "Kuko Salos","CR" = "Kosta Rika","CI" = "Pramanų krantas","HR" = "Kroatija","CU" = "Kuba",
  "CY" = "Kipras","CZ" = "Čekija","DK" = "Danija","DJ" = "Džibutis","DM" = "Dominika","DO" = "Dominikos Respublika","EC" = "Ekvadoras","EG" = "Egiptas","SV" = "Salvadoras",
  "GQ" = "Pusiaujo Gvinėja","ER" = "Eritrėja","EE" = "Estija","ET" = "Etiopija","FK" = "Falklando Salos (Malvinos)","FO" = "Farerų Salos",
  "FJ" = "Fidžis","FI" = "Suomija","FR" = "Prancūzija","GF" = "Prancūzijos Gviana","PF" = "Prancūzijos Polinezija","TF" = "Prancūzijos Pietų teritorijos",
  "GA" = "Gabonas","GM" = "Gambija","GE" = "Gruzija","DE" = "Vokietija","GH" = "Gana","GI" = "Gibraltaras","GR" = "Graikija","GL" = "Grenlandija","GD" = "Grenada","GP" = "Gvadelupė",
  "GU" = "Guamas","GT" = "Gvatemala","GN" = "Gvinėja","GW" = "Bisau Gvinėja","GY" = "Gajana","HT" = "Haitis","HM" = "Herdo ir Makdonaldo Salos","HN" = "Hondūras","HK" = "Honkongas",
  "HU" = "Vengrija","IS" = "Islandija","IN" = "Indija","ID" = "Indonezija","IR" = "Iranas, Islamiškoji Respublika","IQ" = "Irakas","IE" = "Airija",
  "IL" = "Izraelis","IT" = "Italija","JM" = "Jamaika","JP" = "Japonija","JO" = "Jordanija","KZ" = "Kazachstanas","KE" = "Kenija","KI" = "Kiribatis",
  "KP" = "Korėja, Demokratinės Liaudies Respublika","KR" = "Korėja, Respublika","KW" = "Kuveitas","KG" = "Kirgizija","LA" = "Laosas","LV" = "Latvija","LB" = "Libanas","LS" = "Lesotas",
  "LR" = "Liberija","LY" = "Libija","LI" = "Liuksemburgas","LT" = "Lietuva","LU" = "Liuksemburgas","MO" = "Makao","MK" = "Šiaurės Makedonija","MG" = "Madagaskaras","MW" = "Malavis",
  "MY" = "Malaizija","MV" = "Maldyvai","ML" = "Malis","MT" = "Malta","MH" = "Maršalo Salos","MQ" = "Martinika","MR" = "Mauritanija","MU" = "Mauricijus","YT" = "Majotas",
  "MX" = "Meksika","FM" = "Mikronezija, Susijungusios Valstijos","MD" = "Moldova, Respublika","MC" = "Monakas","MN" = "Mongolija","MS" = "Monseratas","MA" = "Marokas","MZ" = "Mozambikas",
  "MM" = "Mianmaras","NA" = "Namibija","NR" = "Nauru","NP" = "Nepalas","NL" = "Nyderlandai","AN" = "Nyderlandų Antilai","NC" = "Naujoji Kaledonija","NZ" = "Naujoji Zelandija","NI" = "Nikaragva",
  "NE" = "Nigeris","NG" = "Nigerija","NU" = "Niujė","NF" = "Norfolko sala","MP" = "Marianos Šiaurinės Salos","NO" = "Norvegija","OM" = "Omanas","PK" = "Pakistanas","PW" = "Palau",
  "PS" = "Palestinietis teritorija, okupuota","PA" = "Panama","PG" = "Papua Naujoji Gvinėja","PY" = "Paragvajus","PE" = "Peru","PH" = "Filipinai","PN" = "Pitkernas",
  "PL" = "Lenkija","PT" = "Portugalija","PR" = "Puerto Rikas","QA" = "Kataras","RE" = "Reunjonas","RO" = "Rumunija","RU" = "Rusija","RW" = "Ruanda","SH" = "Šv. Elena",
  "KN" = "Sent Kitsas ir Nevis","LC" = "Sent Lūcija","PM" = "Sent Pjeras ir Mikelonas","VC" = "Sent Vinsentas ir Grenadinai","WS" = "Samoa","SM" = "San Marinas","ST" = "San Tomė ir Principė",
  "SA" = "Saudo Arabija","SN" = "Senegalas","SC" = "Seišeliai","SL" = "Siera Leonė","SG" = "Singapūras","SK" = "Slovakija","SI" = "Slovėnija","SB" = "Saliamono salos","SO" = "Somalis",
  "ZA" = "Pietų Afrika","GS" = "Pietų Džordžija ir Pietų Sandvičo Salos","ES" = "Ispanija","LK" = "Šri Lanka","SD" = "Sudanas","SR" = "Surinamas","SJ" = "Svalbardas ir Janas Majenas","SZ" = "Svazilendas",
  "SE" = "Švedija", "CH" = "Šveicarija","SY" = "Sirija","TW" = "Taivanas, Kinijos provincija","TJ" = "Tadžikistanas","TZ" = "Tanzanija, Jungtinė Respublika","TH" = "Tailandas","TL" = "Rytų Timoras",
  "TG" = "Togas","TK" = "Tokelau","TO" = "Tonga","TT" = "Trinidadas ir Tobagas","TN" = "Tunisas","TR" = "Turkija","TM" = "Turkmėnistanas","TC" = "Turkso ir Caicoso Salos","TV" = "Tuvalu",
  "UG" = "Uganda","UA" = "Ukraina","AE" = "Jungtiniai Arabų Emyratai","GB" = "Jungtinė Karalystė","US" = "Jungtinės Valstijos","UM" = "Mažosios Tolimosios salos Jungtinėse Valstijose","UY" = "Urugvajus",
  "UZ" = "Uzbekistanas","VU" = "Vanuatu","VE" = "Venesuela","VN" = "Vietnamas","VG" = "Britų Mergelių Salos","VI" = "Jungtinių Valstijų Mergelių Salos","WF" = "Volisas ir Futūna",
  "EH" = "Vakarų Sachara","YE" = "Jemenas","ZM" = "Zambija","ZW" = "Zimbabvė","TOTAL" = "Total"
)
sff_top_5_partners <- filter_top_5_partners("solid_fossil_fuels.csv", siec_mappings, country_mappings)
Peat <- filter_top_5_partners("Peat.csv", siec_mappings, country_mappings)
Wood_pellets <- filter_top_5_partners("Wood pellets.csv", siec_mappings, country_mappings)
Pure_biogasoline <- filter_top_5_partners("Pure biogasoline.csv", siec_mappings, country_mappings)
Natural_gas <- filter_top_5_partners("Natural gas.csv", siec_mappings, country_mappings)
Oil_and_petroleum_products_exc_bio <- filter_top_5_partners("Oil and petroleum products (exc. bio).csv", siec_mappings, country_mappings)
Motor_gasoline_excluding_biofuel <- filter_top_5_partners("Motor gasoline excluding biofuel.csv", siec_mappings, country_mappings)

#-----------------------------------------PIRMASIS GRAFIKAS------------------------------------------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(igraph)

custom_css <- "
body, .box-title, .tab-pane, .sidebar-menu, .main-header .logo, .main-header .navbar {
  font-family: 'Times New Roman', Times, serif !important;
  color: black !important;
}
h1, h2, h3, h4, h5, h6 {
  font-family: 'Times New Roman', Times, serif !important;
  color: black !important;
  font-size: 20px !important;
  font-weight: bold !important;
}
.box-title, .sidebar-menu li a {
  font-size: 18px !important;
}
.leaflet-popup-content {
  font-family: 'Times New Roman', Times, serif !important;
  color: black !important;
}
"

ui <- dashboardPage(
  dashboardHeader(title = "Atsinaujinanti energija"),
  dashboardSidebar(
    width = "250px",
    sidebarMenu(
      menuItem("Žemėlapis", tabName = "energyMap", icon = icon("map")),
      menuItem("Linijinis grafikas", tabName = "lineGraph", icon = icon("line-chart")),
      menuItem("Stulpelinis grafikas", tabName = "edvino", icon = icon("bar-chart")),
      menuItem("Sklaidos diagrama", tabName = "jurgio", icon = icon("scatter-plot")),
      menuItem("Grafas", tabName = "migles", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      tabItem(tabName = "energyMap",
              fluidRow(
                box(width = 6,
                    selectInput("year", "Pasirinkite metus:", choices = unique(summarized_data$Year))
                ),
                box(width = 12, title = "Atsinaujinačios energijos dalis (%)", status = "primary",
                    leafletOutput("map", height = 800)
                )
              )
      ),
      tabItem(tabName = "lineGraph",
              fluidRow(
                box(width = 6,
                    selectInput("measure", "Rodiklis", choices = c("Galutiniai diegimo kaštai" = "Total installed cost", "Technologijos efektyvumas" = "Capacity factor", "Išlygintos elektros energijos sąnaudos" = "LCOE"), selected = "Total installed cost"),
                    selectInput("percentile", "Procentilis", choices = c("5-oji procentilė" = "5th percentile", "Svertinis vidurkis" = "Weighted average", "95-oji procentilė" = "95th percentile"), selected = "Weighted average")
                ),
                box(width = 6,
                    sliderInput("yearRange", "Metų diapazonas", min = 2010, max = 2022, value = c(2010, 2022), step = 1, sep = "")
                ),
                box(width = 12,
                    plotOutput("costPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "edvino",
              fluidRow(
                box(width = 6,
                    selectInput("country", "Pasirinkite šalį:", choices = unique(combined_data1$Country))
                ),
                box(width = 12,
                    plotOutput("barPlot", height = 500)
                ),
                box(width = 12,
                    plotlyOutput("scatterPlot", height = 500)
                )
              )
      ),
      tabItem(tabName = "jurgio",
              fluidRow(
                box(
                  width = 12,
                  selectInput("selected_country", "Pasirinkite šalį:",
                              choices = c("Visos", unique(joined_data_em$country))),  # Add "All" option
                  plotlyOutput("scatter_plot")
                )
              )
      ),
      tabItem(tabName = "migles",
              fluidRow(
                box(width = 6,
                    selectInput("dataset", "Pasirinkite kuro rūšį:", 
                                choices = c(
                                  "Kietasis iškastinis kuras" = "kietojo iškastinio kuro",
                                  "Gamtinės dujos" = "gamtinių dujų",
                                  "Naftos ir naftos produktai (be biokuro)" = "naftos ir naftos produktų (be biokuro)",
                                  "Variklių benzinas (be biokuro)" = "variklių benzino (be biokuro)",
                                  "Durpės" = "durpių",
                                  "Medžio granulės" = "medžio granulių",
                                  "Grynas biobenzinas" = "grynojo biobenzino"
                                ))
                ),
                box(width = 12,
                    uiOutput("yearSliderUI")
                ),
                box(width = 12,
                    uiOutput("valueSliderUI")
                ),
                box(width = 12,
                    actionButton("update", "Atnaujinti tinklą")
                ),
                box(width = 12,
                    div(style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
                        plotOutput("networkPlot", width = "1700px", height = "800px")
                    )
                )
              )
      )
    )
  )
)
color_scale3 <- colorNumeric(
  palette = colorRampPalette(c("blue", "green"))(100),
  domain = c(0, 100)
)
server <- function(input, output) {
  filtered_data <- reactive({
    summarized_data %>% filter(Year == input$year)
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~sqrt(Renewable + Non_Renewable) / 50,
        color = ~color_scale3(Renewable_part_num),
        fillOpacity = 0.7,
        popup = ~paste0(
          "<strong>Šalis: </strong>", `ISO3 code`, "<br>",
          "<strong>Atsinaujinanti: </strong>", Renewable, " GWh<br>",
          "<strong>Neatsinaujinanti: </strong>", Non_Renewable, " GWh<br>",
          "<strong>Atsinaujinančios dalis: </strong>", Renewable_part
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_scale3,
        values = ~Renewable_part_num,
        title = "Atsinaujinančios energijos dalis (%)",
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
      )
  })
  
  output$costPlot <- renderPlot({
    filtered_data <- costs %>%
      filter(Measure == input$measure, Percentile == input$percentile) %>%
      select(Technology, starts_with("X")) %>%
      pivot_longer(cols = starts_with("X"), names_to = "Year", names_prefix = "X", values_to = "Value") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2])
    
    if(input$measure == "Capacity factor") {
      filtered_data$Value <- filtered_data$Value * 100
    }
    
    ggplot(filtered_data, aes(x = Year, y = Value, color = Technology)) +
      geom_line(size = 1.2) +
      labs(
        title = paste(if(input$measure == "Capacity factor") "Technologijos efektyvumo" else 
          if(input$measure == "LCOE") "Išlygintų elektros energijos sąnaudų" else
            "Galutinių diegimo kaštų", "tendencijos"),
        x = "Metai",
        y = if(input$measure == "Capacity factor") "Technologijos efektyvumas (%)" else 
          if(input$measure == "LCOE") "Išlygintos elektros energijos sąnaudos (USD/kWh)" else
            "Galutiniai diegimo kaštai (USD/kW)",
        color = "Technologija"
      ) +
      scale_x_continuous(breaks = seq(input$yearRange[1], input$yearRange[2], by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman"),
        axis.title.y = element_text(size = 16, family = "Times New Roman"),
        axis.text.x = element_text(size = 14, family = "Times New Roman"),
        axis.text.y = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 16, family = "Times New Roman"),
        legend.text = element_text(size = 14, family = "Times New Roman")
      ) +
      if(input$measure == "Capacity factor") ylim(0, 100)
    else if (input$measure == "LCOE") ylim(0, 0.6)
    else ylim(0, 20000)
  })
  

      output$barPlot <- renderPlot({
        plot_data <- combined_data1 %>%
          filter(Country == input$country) %>%
          mutate(Year = as.character(Year)) %>%
          mutate(Difference = Production - Consumption) %>%
          arrange(abs(Difference))
        ggplot(plot_data, aes(x = Year)) +
          geom_bar(aes(y = ifelse(Difference > 0, Production, Consumption) / 1000, fill = ifelse(Difference > 0, "Gamyba", "Suvartojimas")), stat = "identity") +
          geom_bar(aes(y = ifelse(Difference > 0, Consumption, Production) / 1000, fill = ifelse(Difference > 0, "Suvartojimas", "Gamyba")), stat = "identity") +
          labs(y = "Atsinaujinančios energijos kiekis (TWh)", x = "Metai", fill = "Kategorija") +
          scale_fill_manual(values = c("Gamyba" = "lightblue", "Suvartojimas" = "orange")) +
          theme_minimal() +
          ggtitle("Atsinaujinančios energijos suvartojimo ir gamybos kiekiai")+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman"),
        axis.title.y = element_text(size = 16, family = "Times New Roman"),
        axis.text.x = element_text(size = 14, family = "Times New Roman"),
        axis.text.y = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 16, family = "Times New Roman"),
        legend.text = element_text(size = 14, family = "Times New Roman")
      )
  })
  
  
  
  output$scatterPlot <- renderPlotly({
    plot_data <- combined_data2 %>%
      filter(Country == input$country) %>%
      mutate(
        Production_Percentage = round(Production_Percentage, 2),
        Ren_Supply = round(Ren_Supply / 1000, 2)
      )
    p <- ggplot(plot_data, aes(
      x = Production_Percentage, 
      y = Ren_Supply, 
      text = paste(
        "Metai:", Year, 
        "<br>Produkcijos procentas(%):", Production_Percentage, 
        "<br>Atsinaujinančios energijos gamybos kiekiai (TWh):", Ren_Supply
      )
    )) +
      geom_point(size = 2, color = "skyblue") +
      geom_smooth(method = "gam", se = TRUE, color = "red") +
      labs(
        y = "Atsinaujinančios energijos gamybos kiekis (TWh)", 
        x = "Procentas (%)"
      ) +
      theme_minimal() +
      ggtitle("Koreliacija tarp atsinaujinančios energijos gamybos kiekio ir energetinio nepriklausomumo") +
      theme(plot.title = element_text(size = 20, face = "bold", family = "Times New Roman"),
            axis.title.x = element_text(size = 16, family = "Times New Roman"),
            axis.title.y = element_text(size = 16, family = "Times New Roman"),
            axis.text.x = element_text(size = 14, family = "Times New Roman"),
            axis.text.y = element_text(size = 14, family = "Times New Roman"),
            legend.title = element_text(size = 16, family = "Times New Roman"),
            legend.text = element_text(size = 14, family = "Times New Roman"))
    correlation <- cor(plot_data$Production_Percentage, plot_data$Ren_Supply)
    cor_text <- paste("Koreliacijos koeficientas:", round(correlation, 2))
    p <- p + annotate(
      "text", 
      x = (max(plot_data$Production_Percentage) + min(plot_data$Production_Percentage)) / 2, 
      y = max(plot_data$Ren_Supply) + 2,
      label = cor_text, 
      size = 4, 
      color = "blue"
    )
    ggplotly(p, tooltip = "text")
  })
  
  
  output$scatter_plot <- renderPlotly({
    selected_country_data <- if (input$selected_country == "Visos") {
      joined_data_em
    } else {
      joined_data_em %>% filter(country == input$selected_country)
    }
    
    selected_country_data <- selected_country_data %>%
      mutate(
        Emisijos = as.numeric(greenhouse_emissions_per_capita),
        Atsinaujinančios_proc = as.numeric(renewables_share_energy)
      ) %>%
      mutate(
        Emisijos = round(Emisijos, 2),
        Atsinaujinančios_proc = round(Atsinaujinančios_proc, 2)
      )
    
    corr <- cor(selected_country_data$Emisijos, selected_country_data$Atsinaujinančios_proc, use = "complete.obs")
    corr_text <- paste("Koreliacija: ", round(corr, 2))
    
    p <- ggplot(selected_country_data, aes(x = Emisijos, y = Atsinaujinančios_proc)) +
      geom_point(color = "lightblue") +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      labs(
        x = "Emisijos (tonomis/gyv.)",
        y = "Atsinaujinančios energijos dalis %\n tarp visos sunaudojamos energijos",
        title = paste("Atsinaujinančios energijos dalies ir išmetamų\n šiltnamio dujų sklaidos diagrama:", input$selected_country)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", family = "Times New Roman"),
        axis.title.x = element_text(size = 16, family = "Times New Roman"),
        axis.title.y = element_text(size = 16, family = "Times New Roman"),
        axis.text.x = element_text(size = 14, family = "Times New Roman"),
        axis.text.y = element_text(size = 14, family = "Times New Roman"),
        legend.title = element_text(size = 16, family = "Times New Roman"),
        legend.text = element_text(size = 14, family = "Times New Roman")
      )
    
    p <- ggplotly(p)
    
    p <- layout(p, 
                annotations = list(
                  x = 1,
                  y = 1,
                  xref = 'paper',
                  yref = 'paper',
                  text = corr_text,
                  showarrow = FALSE,
                  xanchor = 'right',
                  yanchor = 'top',
                  font = list(size = 16, color = 'black', family = 'Times New Roman')
                ))
    p
  })
  datasets <- list(
    "kietojo iškastinio kuro" = sff_top_5_partners,
    "gamtinių dujų" = Natural_gas,
    "naftos ir naftos produktų (be biokuro)" = Oil_and_petroleum_products_exc_bio,
    "variklių benzino (be biokuro)" = Motor_gasoline_excluding_biofuel,
    "durpių" = Peat,
    "medžio granulių" = Wood_pellets,
    "grynojo biobenzino" = Pure_biogasoline
  )
  observeEvent(input$dataset, {
    selected_dataset <- datasets[[input$dataset]]
    unique_years <- sort(unique(selected_dataset$TIME_PERIOD))
    
    output$yearSliderUI <- renderUI({
      sliderInput("year", "Pasirinkite metus:", min = min(unique_years), max = max(unique_years), value = min(unique_years), step = 1, sep = "")
    })
  })
  observeEvent(input$year, {
    selected_dataset <- datasets[[input$dataset]]
    filtered_data <- selected_dataset %>%
      filter(TIME_PERIOD == input$year)
    max_value <- ceiling(max(filtered_data$OBS_VALUE) / 1000 * 100) / 100
    
    output$valueSliderUI <- renderUI({
      sliderInput("maxValue", "Eksporto vertės intervalas (mln. tonų):", min = 0, max = max_value, value = c(0, max_value), step = 0.01)
    })
  })
  observeEvent(input$update, {
    selected_dataset <- datasets[[input$dataset]]
    
    filtered_data <- selected_dataset %>%
      select(geo, partner, TIME_PERIOD, OBS_VALUE) %>%
      filter(TIME_PERIOD == input$year & OBS_VALUE >= input$maxValue[1] * 1000 & OBS_VALUE <= input$maxValue[2] * 1000)
    
    filtered_data$OBS_VALUE <- filtered_data$OBS_VALUE / 1000
    
    total_export <- filtered_data %>%
      group_by(geo) %>%
      summarise(total_export = sum(OBS_VALUE, na.rm = TRUE))
    
    g <- graph_from_data_frame(d = filtered_data, directed = TRUE)
    E(g)$weight <- filtered_data$OBS_VALUE
    edge.widths <- E(g)$weight / max(E(g)$weight) * 20
    
    vertex_colors <- ifelse(V(g)$name %in% filtered_data$geo, "skyblue", "orange")
    
    vertex_sizes <- ifelse(V(g)$name %in% total_export$geo, 
                           pmax(scale(total_export$total_export[V(g)$name %in% total_export$geo], center = FALSE, scale = max(total_export$total_export) / 24), 9),
                           9)
    
    layout <- layout_with_graphopt(g)
    
    output$networkPlot <- renderPlot({
      par(mar = c(5, 4, 4, 2) + 0.1, family = "serif", col.main = "black", col.lab = "black")  
      plot(g, layout = layout, 
           vertex.color = vertex_colors, 
           edge.color = "grey", 
           vertex.size = vertex_sizes, 
           vertex.frame.color = NA,
           edge.arrow.size = 0.6,
           edge.width = edge.widths,
           edge.curved = 0.3,
           vertex.label.color = "black", 
           vertex.label.cex = 1.5,   
           vertex.label.font = 2)  
      title(main = paste(input$year, "m.", input$dataset, "eksporto/importo ryšiai"), cex.main = 2, font.main = 2, family = "serif") 
      
      par(family = "serif")  # Set legend font to Times New Roman
      legend("topright", legend = c("Top 10 eksportuotojai", "Top 5 partneriai", "Eksporto kiekis mln. tonų"), 
             col = c("skyblue", "orange", "grey"), pch = c(19, 19, NA), pt.cex = c(4, 4, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 4), cex = 1.5, bty = "n", y.intersp = 2, text.font = 2)
    })
  })
}

shinyApp(ui = ui, server = server)
