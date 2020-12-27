library(pacman)
pacman::p_load(telegram.bot)

# file.edit(path.expand(file.path("~", ".Renviron")))
# example. R_TELEGRAM_BOT_RTelegramBot=TOKEN
# Initialize bot
bot <- Bot(token = bot_token("RTelegramBot"))

print(bot$getMe())

updates <- bot$getUpdates()
text <- "Could you type their phone number, please?"
IKM <- InlineKeyboardMarkup(
  inline_keyboard = list(
    list(
      InlineKeyboardButton(1),
      InlineKeyboardButton(2),
      InlineKeyboardButton(3)
    ),
    list(
      InlineKeyboardButton(4),
      InlineKeyboardButton(5),
      InlineKeyboardButton(6)
    ),
    list(
      InlineKeyboardButton(7),
      InlineKeyboardButton(8),
      InlineKeyboardButton(9)
    ),
    list(
      InlineKeyboardButton("*"),
      InlineKeyboardButton(0),
      InlineKeyboardButton("#")
    )
  )
)
# Send Inline Keyboard
bot$sendMessage(Sys.getenv("TG_ME"), text, reply_markup = IKM)
callback_method <- function(bot, update) {
  chat_id <- update$message$chat_id
  bot$sendMessage(chat_id = chat_id, text = "Hello")
}
# No filtering
message_handler <- MessageHandler(callback_method, MessageFilters$all)

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
# 
# updater <- Updater(token = bot_token("RTelegramBot"))
# start <- function(bot, update){
#   bot$sendMessage(chat_id = update$message$chat_id,
#                   text = sprintf("Hello %s!", update$message$from$first_name))
# }
# start_handler <- CommandHandler("start", start)
# updater <- updater + start_handler
# updater$start_polling()
#===================================================