library(pacman)
pacman::p_load(telegram.bot)

# file.edit(path.expand(file.path("~", ".Renviron")))
# example. R_TELEGRAM_BOT_RTelegramBot=TOKEN
# Initialize bot
bot <- Bot(token = bot_token("RTelegramBot"))

print(bot$getMe())

updates <- bot$getUpdates()

chat_id <- updates[[3L]]$from_chat_id()

# Send message
bot$sendMessage(chat_id,
                text = "*A !*",
                parse_mode = "Markdown"
)
# Send photo
bot$sendPhoto(chat_id,
              photo = "./contest/tmp.png"
)
# ==================================================

# updater <- Updater(token = bot_token("RTelegramBot"))
# start <- function(bot, update){
#   bot$sendMessage(chat_id = update$message$chat_id,
#                   text = sprintf("Hello %s!", update$message$from$first_name))
# }
# start_handler <- CommandHandler("start", start)
# updater <- updater + start_handler
# updater$start_polling()
#===================================================