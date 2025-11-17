# English Football Results Dashboard (R Shiny)

An interactive R Shiny dashboard exploring match results and team performance in the top five English football divisions (Premier League, Championship, League One, League Two, and National League) from the 2005/06 season through 2023/24.

The app lets you filter by team and match location (home vs away) to investigate:
- How total shots relate to goals scored
- How often each team wins, draws, or loses, split by home/away

---

## Project Structure

```text
.
├── app.R        # Main Shiny application
├── eng.csv      # Match-level data for English leagues (2005–2024)
└── README.md    # Project description and usage instructions
