#!/usr/bin/env python3.7

import asyncio
import iterm2

async def changeTheme(connection,parts,tmux_connections=[]):
    theme_dark = "Tango Dark"
    theme_light = "Tango Light";

    if "dark" in parts:
        preset = await iterm2.ColorPreset.async_get(connection, theme_dark)
        [await c.async_send_command("set-option status-fg black") for c in tmux_connections]
    else:
        preset = await iterm2.ColorPreset.async_get(connection, theme_light)
        [await c.async_send_command("set-option status-fg white") for c in tmux_connections]

    # Update the list of all profiles and iterate over them.
    profiles=await iterm2.PartialProfile.async_query(connection)
    for partial in profiles:
        # Fetch the full profile and then set the color preset in it.
        profile = await partial.async_get_full_profile()
        await profile.async_set_color_preset(preset)

async def main(connection):

    app = await iterm2.async_get_app(connection)
    window = app.current_window
    initial_theme = await app.async_get_theme()
    tmux_connections = await iterm2.async_get_tmux_connections(connection)
    await changeTheme(connection,initial_theme,tmux_connections)

    async with iterm2.VariableMonitor(connection, iterm2.VariableScopes.APP, "effectiveTheme", None) as mon:
        while True:
            # Block until theme changes
            theme = await mon.async_get()
            # Themes have space-delimited attributes, one of which will be light or dark.
            parts = theme.split(" ")

            await changeTheme(connection,parts)


iterm2.run_forever(main)
