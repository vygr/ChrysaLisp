(defq timezones (list 
 (list "DST" -12 nil "(UTC-12:00) International Date Line West" 
 (list "Etc/GMT+12")) 
 (list "U" -11 nil "(UTC-11:00) Coordinated Universal Time-11" 
 (list "Etc/GMT+11" "Pacific/Midway" "Pacific/Niue" "Pacific/Pago_Pago")) 
 (list "HST" -10 nil "(UTC-10:00) Hawaii" 
 (list "Etc/GMT+10" "Pacific/Honolulu" "Pacific/Johnston" "Pacific/Rarotonga" "Pacific/Tahiti")) 
 (list "AKDT" -8 t "(UTC-09:00) Alaska" 
 (list "America/Anchorage" "America/Juneau" "America/Nome" "America/Sitka" "America/Yakutat")) 
 (list "PDT" -7 t "(UTC-08:00) Baja California" 
 (list "America/Santa_Isabel")) 
 (list "PDT" -7 t "(UTC-07:00) Pacific Time (US & Canada)" 
 (list "America/Dawson" "America/Los_Angeles" "America/Tijuana" "America/Vancouver" "America/Whitehorse")) 
 (list "PST" -8 nil "(UTC-08:00) Pacific Time (US & Canada)" 
 (list "America/Dawson" "America/Los_Angeles" "America/Tijuana" "America/Vancouver" "America/Whitehorse" "PST8PDT")) 
 (list "UMST" -7 nil "(UTC-07:00) Arizona" 
 (list "America/Creston" "America/Dawson_Creek" "America/Hermosillo" "America/Phoenix" "Etc/GMT+7")) 
 (list "MDT" -6 t "(UTC-07:00) Chihuahua, La Paz, Mazatlan" 
 (list "America/Chihuahua" "America/Mazatlan")) 
 (list "MDT" -6 t "(UTC-07:00) Mountain Time (US & Canada)" 
 (list "America/Boise" "America/Cambridge_Bay" "America/Denver" "America/Edmonton" "America/Inuvik" "America/Ojinaga" "America/Yellowknife" "MST7MDT")) 
 (list "CAST" -6 nil "(UTC-06:00) Central America" 
 (list "America/Belize" "America/Costa_Rica" "America/El_Salvador" "America/Guatemala" "America/Managua" "America/Tegucigalpa" "Etc/GMT+6" "Pacific/Galapagos")) 
 (list "CDT" -5 t "(UTC-06:00) Central Time (US & Canada)" 
 (list "America/Chicago" "America/Indiana/Knox" "America/Indiana/Tell_City" "America/Matamoros" "America/Menominee" "America/North_Dakota/Beulah" "America/North_Dakota/Center" "America/North_Dakota/New_Salem" "America/Rainy_River" "America/Rankin_Inlet" "America/Resolute" "America/Winnipeg" "CST6CDT")) 
 (list "CDT" -5 t "(UTC-06:00) Guadalajara, Mexico City, Monterrey" 
 (list "America/Bahia_Banderas" "America/Cancun" "America/Merida" "America/Mexico_City" "America/Monterrey")) 
 (list "CCST" -6 nil "(UTC-06:00) Saskatchewan" 
 (list "America/Regina" "America/Swift_Current")) 
 (list "SPST" -5 nil "(UTC-05:00) Bogota, Lima, Quito" 
 (list "America/Bogota" "America/Cayman" "America/Coral_Harbour" "America/Eirunepe" "America/Guayaquil" "America/Jamaica" "America/Lima" "America/Panama" "America/Rio_Branco" "Etc/GMT+5")) 
 (list "EDT" -4 t "(UTC-05:00) Eastern Time (US & Canada)" 
 (list "America/Detroit" "America/Havana" "America/Indiana/Petersburg" "America/Indiana/Vincennes" "America/Indiana/Winamac" "America/Iqaluit" "America/Kentucky/Monticello" "America/Louisville" "America/Montreal" "America/Nassau" "America/New_York" "America/Nipigon" "America/Pangnirtung" "America/Port-au-Prince" "America/Thunder_Bay" "America/Toronto" "EST5EDT")) 
 (list "UEDT" -4 t "(UTC-05:00) Indiana (East)" 
 (list "America/Indiana/Marengo" "America/Indiana/Vevay" "America/Indianapolis")) 
 (list "VST" -4.5 nil "(UTC-04:30) Caracas" 
 (list "America/Caracas")) 
 (list "PYT" -4 nil "(UTC-04:00) Asuncion" 
 (list "America/Asuncion")) 
 (list "ADT" -3 t "(UTC-04:00) Atlantic Time (Canada)" 
 (list "America/Glace_Bay" "America/Goose_Bay" "America/Halifax" "America/Moncton" "America/Thule" "Atlantic/Bermuda")) 
 (list "CBST" -4 nil "(UTC-04:00) Cuiaba" 
 (list "America/Campo_Grande" "America/Cuiaba")) 
 (list "SWST" -4 nil "(UTC-04:00) Georgetown, La Paz, Manaus, San Juan" 
 (list "America/Anguilla" "America/Antigua" "America/Aruba" "America/Barbados" "America/Blanc-Sablon" "America/Boa_Vista" "America/Curacao" "America/Dominica" "America/Grand_Turk" "America/Grenada" "America/Guadeloupe" "America/Guyana" "America/Kralendijk" "America/La_Paz" "America/Lower_Princes" "America/Manaus" "America/Marigot" "America/Martinique" "America/Montserrat" "America/Port_of_Spain" "America/Porto_Velho" "America/Puerto_Rico" "America/Santo_Domingo" "America/St_Barthelemy" "America/St_Kitts" "America/St_Lucia" "America/St_Thomas" "America/St_Vincent" "America/Tortola" "Etc/GMT+4")) 
 (list "PSST" -4 nil "(UTC-04:00) Santiago" 
 (list "America/Santiago" "Antarctica/Palmer")) 
 (list "NDT" -2.5 t "(UTC-03:30) Newfoundland" 
 (list "America/St_Johns")) 
 (list "ESAST" -3 nil "(UTC-03:00) Brasilia" 
 (list "America/Sao_Paulo")) 
 (list "AST" -3 nil "(UTC-03:00) Buenos Aires" 
 (list "America/Argentina/La_Rioja" "America/Argentina/Rio_Gallegos" "America/Argentina/Salta" "America/Argentina/San_Juan" "America/Argentina/San_Luis" "America/Argentina/Tucuman" "America/Argentina/Ushuaia" "America/Buenos_Aires" "America/Catamarca" "America/Cordoba" "America/Jujuy" "America/Mendoza")) 
 (list "SEST" -3 nil "(UTC-03:00) Cayenne, Fortaleza" 
 (list "America/Araguaina" "America/Belem" "America/Cayenne" "America/Fortaleza" "America/Maceio" "America/Paramaribo" "America/Recife" "America/Santarem" "Antarctica/Rothera" "Atlantic/Stanley" "Etc/GMT+3")) 
 (list "GDT" -3 t "(UTC-03:00) Greenland" 
 (list "America/Godthab")) 
 (list "MST" -3 nil "(UTC-03:00) Montevideo" 
 (list "America/Montevideo")) 
 (list "BST" -3 nil "(UTC-03:00) Salvador" 
 (list "America/Bahia")) 
 (list "U" -2 nil "(UTC-02:00) Coordinated Universal Time-02" 
 (list "America/Noronha" "Atlantic/South_Georgia" "Etc/GMT+2")) 
 (list "MDT" -1 t "(UTC-02:00) Mid-Atlantic - Old" 
 (list "value" "abbr" "offset" "isdst" "text" "utc" "America/Scoresbysund" "Atlantic/Azores")) 
 (list "CVST" -1 nil "(UTC-01:00) Cape Verde Is." 
 (list "Atlantic/Cape_Verde" "Etc/GMT+1")) 
 (list "MDT" 1 t "(UTC) Casablanca" 
 (list "Africa/Casablanca" "Africa/El_Aaiun")) 
 (list "UTC" 0 nil "(UTC) Coordinated Universal Time" 
 (list "America/Danmarkshavn" "Etc/GMT")) 
 (list "GMT" 0 nil "(UTC) Edinburgh, London" 
 (list "Europe/Isle_of_Man" "Europe/Guernsey" "Europe/Jersey" "Europe/London")) 
 (list "BST" 1 t "(UTC+01:00) Edinburgh, London" 
 (list "Europe/Isle_of_Man" "Europe/Guernsey" "Europe/Jersey" "Europe/London")) 
 (list "GDT" 1 t "(UTC) Dublin, Lisbon" 
 (list "Atlantic/Canary" "Atlantic/Faeroe" "Atlantic/Madeira" "Europe/Dublin" "Europe/Lisbon")) 
 (list "GST" 0 nil "(UTC) Monrovia, Reykjavik" 
 (list "Africa/Abidjan" "Africa/Accra" "Africa/Bamako" "Africa/Banjul" "Africa/Bissau" "Africa/Conakry" "Africa/Dakar" "Africa/Freetown" "Africa/Lome" "Africa/Monrovia" "Africa/Nouakchott" "Africa/Ouagadougou" "Africa/Sao_Tome" "Atlantic/Reykjavik" "Atlantic/St_Helena")) 
 (list "WEDT" 2 t "(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna" 
 (list "Arctic/Longyearbyen" "Europe/Amsterdam" "Europe/Andorra" "Europe/Berlin" "Europe/Busingen" "Europe/Gibraltar" "Europe/Luxembourg" "Europe/Malta" "Europe/Monaco" "Europe/Oslo" "Europe/Rome" "Europe/San_Marino" "Europe/Stockholm" "Europe/Vaduz" "Europe/Vatican" "Europe/Vienna" "Europe/Zurich")) 
 (list "CEDT" 2 t "(UTC+01:00) Belgrade, Bratislava, Budapest, Ljubljana, Prague" 
 (list "Europe/Belgrade" "Europe/Bratislava" "Europe/Budapest" "Europe/Ljubljana" "Europe/Podgorica" "Europe/Prague" "Europe/Tirane")) 
 (list "RDT" 2 t "(UTC+01:00) Brussels, Copenhagen, Madrid, Paris" 
 (list "Africa/Ceuta" "Europe/Brussels" "Europe/Copenhagen" "Europe/Madrid" "Europe/Paris")) 
 (list "CEDT" 2 t "(UTC+01:00) Sarajevo, Skopje, Warsaw, Zagreb" 
 (list "Europe/Sarajevo" "Europe/Skopje" "Europe/Warsaw" "Europe/Zagreb")) 
 (list "WCAST" 1 nil "(UTC+01:00) West Central Africa" 
 (list "Africa/Algiers" "Africa/Bangui" "Africa/Brazzaville" "Africa/Douala" "Africa/Kinshasa" "Africa/Lagos" "Africa/Libreville" "Africa/Luanda" "Africa/Malabo" "Africa/Ndjamena" "Africa/Niamey" "Africa/Porto-Novo" "Africa/Tunis" "Etc/GMT-1")) 
 (list "NST" 1 nil "(UTC+01:00) Windhoek" 
 (list "Africa/Windhoek")) 
 (list "GDT" 3 t "(UTC+02:00) Athens, Bucharest" 
 (list "Asia/Nicosia" "Europe/Athens" "Europe/Bucharest" "Europe/Chisinau")) 
 (list "MEDT" 3 t "(UTC+02:00) Beirut" 
 (list "Asia/Beirut")) 
 (list "EST" 2 nil "(UTC+02:00) Cairo" 
 (list "Africa/Cairo")) 
 (list "SDT" 3 t "(UTC+02:00) Damascus" 
 (list "Asia/Damascus")) 
 (list "EEDT" 3 t "(UTC+02:00) E. Europe" 
 (list "Asia/Nicosia" "Europe/Athens" "Europe/Bucharest" "Europe/Chisinau" "Europe/Helsinki" "Europe/Kiev" "Europe/Mariehamn" "Europe/Nicosia" "Europe/Riga" "Europe/Sofia" "Europe/Tallinn" "Europe/Uzhgorod" "Europe/Vilnius" "Europe/Zaporozhye")) 
 (list "SAST" 2 nil "(UTC+02:00) Harare, Pretoria" 
 (list "Africa/Blantyre" "Africa/Bujumbura" "Africa/Gaborone" "Africa/Harare" "Africa/Johannesburg" "Africa/Kigali" "Africa/Lubumbashi" "Africa/Lusaka" "Africa/Maputo" "Africa/Maseru" "Africa/Mbabane" "Etc/GMT-2")) 
 (list "FDT" 3 t "(UTC+02:00) Helsinki, Kyiv, Riga, Sofia, Tallinn, Vilnius" 
 (list "Europe/Helsinki" "Europe/Kiev" "Europe/Mariehamn" "Europe/Riga" "Europe/Sofia" "Europe/Tallinn" "Europe/Uzhgorod" "Europe/Vilnius" "Europe/Zaporozhye")) 
 (list "TDT" 3 nil "(UTC+03:00) Istanbul" 
 (list "Europe/Istanbul")) 
 (list "JDT" 3 t "(UTC+02:00) Jerusalem" 
 (list "Asia/Jerusalem")) 
 (list "LST" 2 nil "(UTC+02:00) Tripoli" 
 (list "Africa/Tripoli")) 
 (list "JST" 3 nil "(UTC+03:00) Amman" 
 (list "Asia/Amman")) 
 (list "AST" 3 nil "(UTC+03:00) Baghdad" 
 (list "Asia/Baghdad")) 
 (list "KST" 3 nil "(UTC+02:00) Kaliningrad" 
 (list "Europe/Kaliningrad")) 
 (list "AST" 3 nil "(UTC+03:00) Kuwait, Riyadh" 
 (list "Asia/Aden" "Asia/Bahrain" "Asia/Kuwait" "Asia/Qatar" "Asia/Riyadh")) 
 (list "EAST" 3 nil "(UTC+03:00) Nairobi" 
 (list "Africa/Addis_Ababa" "Africa/Asmera" "Africa/Dar_es_Salaam" "Africa/Djibouti" "Africa/Juba" "Africa/Kampala" "Africa/Khartoum" "Africa/Mogadishu" "Africa/Nairobi" "Antarctica/Syowa" "Etc/GMT-3" "Indian/Antananarivo" "Indian/Comoro" "Indian/Mayotte")) 
 (list "MSK" 3 nil "(UTC+03:00) Moscow, St. Petersburg, Volgograd, Minsk" 
 (list "Europe/Kirov" "Europe/Moscow" "Europe/Simferopol" "Europe/Volgograd" "Europe/Minsk")) 
 (list "SAMT" 4 nil "(UTC+04:00) Samara, Ulyanovsk, Saratov" 
 (list "Europe/Astrakhan" "Europe/Samara" "Europe/Ulyanovsk")) 
 (list "IDT" 4.5 t "(UTC+03:30) Tehran" 
 (list "Asia/Tehran")) 
 (list "AST" 4 nil "(UTC+04:00) Abu Dhabi, Muscat" 
 (list "Asia/Dubai" "Asia/Muscat" "Etc/GMT-4")) 
 (list "ADT" 5 t "(UTC+04:00) Baku" 
 (list "Asia/Baku")) 
 (list "MST" 4 nil "(UTC+04:00) Port Louis" 
 (list "Indian/Mahe" "Indian/Mauritius" "Indian/Reunion")) 
 (list "GET" 4 nil "(UTC+04:00) Tbilisi" 
 (list "Asia/Tbilisi")) 
 (list "CST" 4 nil "(UTC+04:00) Yerevan" 
 (list "Asia/Yerevan")) 
 (list "AST" 4.5 nil "(UTC+04:30) Kabul" 
 (list "Asia/Kabul")) 
 (list "WAST" 5 nil "(UTC+05:00) Ashgabat, Tashkent" 
 (list "Antarctica/Mawson" "Asia/Aqtau" "Asia/Aqtobe" "Asia/Ashgabat" "Asia/Dushanbe" "Asia/Oral" "Asia/Samarkand" "Asia/Tashkent" "Etc/GMT-5" "Indian/Kerguelen" "Indian/Maldives")) 
 (list "YEKT" 5 nil "(UTC+05:00) Yekaterinburg" 
 (list "Asia/Yekaterinburg")) 
 (list "PKT" 5 nil "(UTC+05:00) Islamabad, Karachi" 
 (list "Asia/Karachi")) 
 (list "IST" 5.5 nil "(UTC+05:30) Chennai, Kolkata, Mumbai, New Delhi" 
 (list "Asia/Kolkata")) 
 (list "SLST" 5.5 nil "(UTC+05:30) Sri Jayawardenepura" 
 (list "Asia/Colombo")) 
 (list "NST" 5.75 nil "(UTC+05:45) Kathmandu" 
 (list "Asia/Kathmandu")) 
 (list "CAST" 6 nil "(UTC+06:00) Nur-Sultan (Astana)" 
 (list "Antarctica/Vostok" "Asia/Almaty" "Asia/Bishkek" "Asia/Qyzylorda" "Asia/Urumqi" "Etc/GMT-6" "Indian/Chagos")) 
 (list "BST" 6 nil "(UTC+06:00) Dhaka" 
 (list "Asia/Dhaka" "Asia/Thimphu")) 
 (list "MST" 6.5 nil "(UTC+06:30) Yangon (Rangoon)" 
 (list "Asia/Rangoon" "Indian/Cocos")) 
 (list "SAST" 7 nil "(UTC+07:00) Bangkok, Hanoi, Jakarta" 
 (list "Antarctica/Davis" "Asia/Bangkok" "Asia/Hovd" "Asia/Jakarta" "Asia/Phnom_Penh" "Asia/Pontianak" "Asia/Saigon" "Asia/Vientiane" "Etc/GMT-7" "Indian/Christmas")) 
 (list "NCAST" 7 nil "(UTC+07:00) Novosibirsk" 
 (list "Asia/Novokuznetsk" "Asia/Novosibirsk" "Asia/Omsk")) 
 (list "CST" 8 nil "(UTC+08:00) Beijing, Chongqing, Hong Kong, Urumqi" 
 (list "Asia/Hong_Kong" "Asia/Macau" "Asia/Shanghai")) 
 (list "NAST" 8 nil "(UTC+08:00) Krasnoyarsk" 
 (list "Asia/Krasnoyarsk")) 
 (list "MPST" 8 nil "(UTC+08:00) Kuala Lumpur, Singapore" 
 (list "Asia/Brunei" "Asia/Kuala_Lumpur" "Asia/Kuching" "Asia/Makassar" "Asia/Manila" "Asia/Singapore" "Etc/GMT-8")) 
 (list "WAST" 8 nil "(UTC+08:00) Perth" 
 (list "Antarctica/Casey" "Australia/Perth")) 
 (list "TST" 8 nil "(UTC+08:00) Taipei" 
 (list "Asia/Taipei")) 
 (list "UST" 8 nil "(UTC+08:00) Ulaanbaatar" 
 (list "Asia/Choibalsan" "Asia/Ulaanbaatar")) 
 (list "NAEST" 8 nil "(UTC+08:00) Irkutsk" 
 (list "Asia/Irkutsk")) 
 (list "JST" 9 nil "(UTC+09:00) Osaka, Sapporo, Tokyo" 
 (list "Asia/Dili" "Asia/Jayapura" "Asia/Tokyo" "Etc/GMT-9" "Pacific/Palau")) 
 (list "KST" 9 nil "(UTC+09:00) Seoul" 
 (list "Asia/Pyongyang" "Asia/Seoul")) 
 (list "CAST" 9.5 nil "(UTC+09:30) Adelaide" 
 (list "Australia/Adelaide" "Australia/Broken_Hill")) 
 (list "ACST" 9.5 nil "(UTC+09:30) Darwin" 
 (list "Australia/Darwin")) 
 (list "EAST" 10 nil "(UTC+10:00) Brisbane" 
 (list "Australia/Brisbane" "Australia/Lindeman")) 
 (list "AEST" 10 nil "(UTC+10:00) Canberra, Melbourne, Sydney" 
 (list "Australia/Melbourne" "Australia/Sydney")) 
 (list "WPST" 10 nil "(UTC+10:00) Guam, Port Moresby" 
 (list "Antarctica/DumontDUrville" "Etc/GMT-10" "Pacific/Guam" "Pacific/Port_Moresby" "Pacific/Saipan" "Pacific/Truk")) 
 (list "TST" 10 nil "(UTC+10:00) Hobart" 
 (list "Australia/Currie" "Australia/Hobart")) 
 (list "YST" 9 nil "(UTC+09:00) Yakutsk" 
 (list "Asia/Chita" "Asia/Khandyga" "Asia/Yakutsk")) 
 (list "CPST" 11 nil "(UTC+11:00) Solomon Is., New Caledonia" 
 (list "Antarctica/Macquarie" "Etc/GMT-11" "Pacific/Efate" "Pacific/Guadalcanal" "Pacific/Kosrae" "Pacific/Noumea" "Pacific/Ponape")) 
 (list "VST" 11 nil "(UTC+11:00) Vladivostok" 
 (list "Asia/Sakhalin" "Asia/Ust-Nera" "Asia/Vladivostok")) 
 (list "NZST" 12 nil "(UTC+12:00) Auckland, Wellington" 
 (list "Antarctica/McMurdo" "Pacific/Auckland")) 
 (list "U" 12 nil "(UTC+12:00) Coordinated Universal Time+12" 
 (list "Etc/GMT-12" "Pacific/Funafuti" "Pacific/Kwajalein" "Pacific/Majuro" "Pacific/Nauru" "Pacific/Tarawa" "Pacific/Wake" "Pacific/Wallis")) 
 (list "FST" 12 nil "(UTC+12:00) Fiji" 
 (list "Pacific/Fiji")) 
 (list "MST" 12 nil "(UTC+12:00) Magadan" 
 (list "Asia/Anadyr" "Asia/Kamchatka" "Asia/Magadan" "Asia/Srednekolymsk")) 
 (list "KDT" 13 t "(UTC+12:00) Petropavlovsk-Kamchatsky - Old" 
 (list "Asia/Kamchatka")) 
 (list "TST" 13 nil "(UTC+13:00) Nuku'alofa" 
 (list "Etc/GMT-13" "Pacific/Enderbury" "Pacific/Fakaofo" "Pacific/Tongatapu")) 
 (list "SST" 13 nil "(UTC+13:00) Samoa" 
 (list "Pacific/Apia"))))
