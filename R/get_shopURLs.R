get_shopURLs = function(shopURLs_ja, lang = "en") {
        # Converts the URLs to Japanese sites to URLs to sites in user specified
        #       language
        # Returns a character vector of URLs of sites in user specified language
        #
        # shopURLs_ja: character vector of URLs to sites in Japanese
        # lang: string, language of values: "en", "cn", "tw", "kr"
        
        tryCatch({ lang %in% c("en", "cn", "tw", "kr")
        }, error = function(e) "Language is not supported. Please enter one of
                        these languages: 'en', 'cn', 'tw', 'kr'"
        )
        gsub("\\.com", paste("\\.com", lang, sep="/"), shopURLs_ja)
}


