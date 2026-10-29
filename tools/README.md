# Lookup tables (public data)

| File | Content | Source |
|---|---|---|
| `pc4_gemeente_nl.csv` | Dutch 4-digit postcode → municipality (2025) | CBS, "Buurt, wijk en gemeente 2025 voor postcode huisnummer" |
| `pc4_province_nl.csv` | Dutch 4-digit postcode → province (2025) | combined with CBS table 86059NED "Gebieden in Nederland 2025" |

Each 4-digit postcode is assigned to the municipality with the most addresses
(21 of 4,072 postcodes span more than one municipality).
Created with `tools/make_nl_lookup.R` on 13.10.2026. Only needed to rebuild the tables.
Source: Statistics Netherlands (CBS), [licence, e.g. CC BY 4.0 – please check].
