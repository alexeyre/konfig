from notion.client import NotionClient
from datetime import datetime
from notion.block import PageBlock, SubsubheaderBlock, TextBlock, DividerBlock
from argparse import ArgumentParser, RawTextHelpFormatter

from emoji import emojize

ICONS = [
    "new_moon",
    "waxing_crescent_moon",
    "first_quarter_moon",
    "waxing_gibbous_moon",
    "full_moon",
    "waning_gibbous_mon",
    "last_quarter_moon",
    "waning_cresent_moon",
]

t = datetime.now()


def get_today_page(log_page):
    today = t.strftime("%Y-%m-%d")
    for child in log_page.children:
        if child.title == today:
            return child
    today_page = log_page.children.add_new(PageBlock, title=today)
    today_page.move_to(log_page, "first-child")
    today_page.icon = get_icon_by_day()
    return today_page


def get_icon_by_day():
    day = t.day
    icon_to_use = ICONS[day % len(ICONS)]
    print(emojize("ICON CHOSEN: %s, :%s:" % (icon_to_use, icon_to_use)))
    return emojize(":%s:" % icon_to_use)


def main(note, api_key, log_page_id):
    client = NotionClient(token_v2=api_key)
    log_page = client.get_block(log_page_id)
    if log_page is None:
        return
    today_page = get_today_page(log_page)
    now = t.strftime("%H:%M:%S")
    now_block = today_page.children.add_new(SubsubheaderBlock, title=now)
    now_block.move_to(today_page, "first-child")
    text_entry = today_page.children.add_new(TextBlock, title=note)
    text_entry.move_to(now_block, "after")
    div_block = today_page.children.add_new(DividerBlock)
    div_block.move_to(text_entry, "after")


if __name__ == "__main__":
    arg = ArgumentParser(formatter_class=RawTextHelpFormatter)
    arg.add_argument(
        "--api-key", "-k", help="Notion token_v2 cookie value", required=True
    )
    arg.add_argument(
        "--log-page",
        "-l",
        help="Notion ID of the page to append logs to",
        required=False,
        default="bbc2d550ac9f4670aa2d37b5e2902ba3",
    )
    arg.add_argument("note")
    args = arg.parse_args()
    main(args.note, args.api_key, args.log_page)
