# =============================================================================
# Boggle Solver - single-file Shiny app
#
# First run (locally): downloads the dictionary + frequency list and caches
# them in ./data/. Commit the data/ folder so the host never has to rebuild.
# =============================================================================

library(shiny)

# ---- Config -----------------------------------------------------------------

DICT_URL <- "https://gist.githubusercontent.com/deostroll/7693b6f3d48b44a89ee5f57bf750bd32/raw/426f564cf73b4c87d2b2c46ccded8a5b98658ce1/dictionary.txt"
FREQ_URL <- "https://norvig.com/ngrams/count_1w.txt"
DATA_DIR <- "data"

# Commonness levels: label -> max frequency rank allowed (rank among valid
# dictionary words only, 1 = most common). These cutoffs are starting guesses;
# tune them by looking at words near each boundary.
LEVELS <- c(
  "Most"     = 30000,
  "All"      = Inf
)
DEFAULT_LEVEL <- 1  # "Most"


# ---- Data prep (runs once, then cached) ---------------------------------------

# Builds two hash-based "sets" for fast lookups:
#   words    = every valid word
#   prefixes = every possible start of a valid word (used to prune the search)
make_sets_fast <- function(words) {
  max_len <- max(nchar(words))
  prefixes <- unique(unlist(lapply(seq_len(max_len), function(k) {
    unique(substr(words, 1, k))
  })))
  
  to_env <- function(x) {
    list2env(setNames(as.list(rep(TRUE, length(x))), x),
             envir = new.env(hash = TRUE))
  }
  list(words = to_env(words), prefixes = to_env(prefixes))
}

build_data <- function() {
  message("Building data (first run only)...")
  
  # Dictionary: keep the first token on each line, uppercase letters only, 3+ long
  raw   <- readLines(DICT_URL, warn = FALSE)
  words <- toupper(sub("[[:space:]].*$", "", trimws(raw)))
  words <- unique(words[grepl("^[A-Z]{3,}$", words)])
  
  # Frequency list -> rank among valid dictionary words
  # Use a local copy of Norvig's file if present (download it in your browser
  # and save as count_1w.txt next to app.R); otherwise download it, posing as
  # a browser because the site can serve a web page to R's default user agent.
  old_ua <- options(HTTPUserAgent = paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "(KHTML, like Gecko) Chrome/120.0 Safari/537.36"))
  on.exit(options(old_ua), add = TRUE)
  
  freq_source <- if (file.exists("count_1w.txt")) "count_1w.txt" else FREQ_URL
  lines <- readLines(freq_source, warn = FALSE)
  if (any(grepl("<html|<!doctype", head(lines, 5), ignore.case = TRUE))) {
    stop("Got a web page instead of the frequency list. Download ", FREQ_URL,
         " in your browser, save it as count_1w.txt next to app.R, and rerun.")
  }
  parts <- strsplit(trimws(lines), "[[:space:]]+")
  parts <- parts[lengths(parts) == 2]
  freq <- data.frame(
    word  = toupper(vapply(parts, `[`, "", 1)),
    count = suppressWarnings(as.numeric(vapply(parts, `[`, "", 2))),
    stringsAsFactors = FALSE
  )
  freq <- freq[!is.na(freq$count), ]
  stopifnot("Frequency list parsed to 0 rows - check FREQ_URL" = nrow(freq) > 0)
  
  freq <- freq[freq$word %in% words, ]
  stopifnot("No frequency words matched the dictionary" = nrow(freq) > 0)
  freq <- freq[!duplicated(freq$word), ]
  freq <- freq[order(-freq$count), ]
  freq$rank <- seq_len(nrow(freq))
  freq <- freq[, c("word", "rank")]
  
  list(sets = make_sets_fast(words), freq = freq)
}

load_data <- function() {
  sets_file <- file.path(DATA_DIR, "sets.rds")
  freq_file <- file.path(DATA_DIR, "freq.rds")
  
  if (!(file.exists(sets_file) && file.exists(freq_file))) {
    d <- build_data()
    dir.create(DATA_DIR, showWarnings = FALSE)
    saveRDS(d$sets, sets_file)
    saveRDS(d$freq, freq_file)
    return(d)
  }
  list(sets = readRDS(sets_file), freq = readRDS(freq_file))
}

# Top level (outside server) = runs once per R process, shared by all visitors
DATA <- load_data()


# ---- Solver -------------------------------------------------------------------

dfs <- function(board, r, c, current, visited, sets, found) {
  # Off the board?
  if (r < 1 || r > nrow(board) || c < 1 || c > ncol(board)) return(invisible())
  # Tile already used in this path?
  if (visited[r, c]) return(invisible())
  
  current <- paste0(current, board[r, c])
  
  # Prune: no word starts with this string
  if (!exists(current, envir = sets$prefixes, inherits = FALSE)) return(invisible())
  
  # Record real words (dictionary is already 3+ letters)
  if (exists(current, envir = sets$words, inherits = FALSE)) {
    assign(current, TRUE, envir = found)
  }
  
  # R copies `visited` on modify, so backtracking is automatic
  visited[r, c] <- TRUE
  for (dr in -1:1) {
    for (dc in -1:1) {
      if (dr == 0 && dc == 0) next
      dfs(board, r + dr, c + dc, current, visited, sets, found)
    }
  }
}

solve_boggle <- function(board, sets) {
  found   <- new.env(hash = TRUE)
  visited <- matrix(FALSE, nrow = nrow(board), ncol = ncol(board))
  for (r in seq_len(nrow(board))) {
    for (c in seq_len(ncol(board))) {
      dfs(board, r, c, "", visited, sets, found)
    }
  }
  sort(ls(found))
}


# ---- UI helpers ---------------------------------------------------------------

# A simple segmented control built from buttons; JS (below) reports the
# selected value to Shiny as input$<id>.
seg <- function(id, labels, values, selected) {
  div(
    id = id, class = "seg",
    mapply(
      function(l, v) {
        tags$button(type = "button",
                    class = if (v == selected) "on",
                    `data-value` = v, l)
      },
      labels, values, SIMPLIFY = FALSE, USE.NAMES = FALSE
    )
  )
}

css <- "
  body { background:#fafafa; color:#222; }
  .wrap { max-width:460px; margin:0 auto; padding:24px 16px 60px; }
  h1 { font-size:26px; text-align:center; margin:8px 0 20px; font-weight:700; }
  .board { display:grid; grid-template-columns:repeat(4,1fr); gap:8px;
           max-width:300px; margin:0 auto 16px; }
  .cell { width:100%; aspect-ratio:1; text-align:center; font-size:28px;
          font-weight:700; border:2px solid #cfcfcf; border-radius:10px;
          background:#fff; padding:0; text-transform:uppercase;
          caret-color:transparent; }
  .cell:focus { outline:none; border-color:#2b6cb0; box-shadow:0 0 0 3px #bee3f8; }
  .actions { display:flex; gap:8px; justify-content:center; margin-bottom:8px; }
  .btn-main { background:#2b6cb0; color:#fff; border:0; border-radius:8px;
              padding:10px 26px; font-size:16px; font-weight:600; }
  .btn-main:hover { background:#2c5282; }
  .btn-quiet { background:transparent; color:#666; border:1px solid #cfcfcf;
               border-radius:8px; padding:10px 16px; font-size:16px; }
  .msg { text-align:center; color:#b83232; min-height:20px; margin-bottom:8px; }
  .controls { margin:18px 0 6px; }
  .ctl-label { font-size:12px; text-transform:uppercase; letter-spacing:.05em;
               color:#777; margin:10px 0 4px; }
  .seg { display:flex; }
  .seg button { flex:1; border:1px solid #cfcfcf; background:#fff; color:#444;
                padding:7px 4px; font-size:14px; margin-left:-1px; cursor:pointer; }
  .seg button:first-child { border-radius:8px 0 0 8px; margin-left:0; }
  .seg button:last-child  { border-radius:0 8px 8px 0; }
  .seg button.on { background:#2b6cb0; border-color:#2b6cb0; color:#fff; }
  .count { color:#666; font-size:14px; margin:14px 0 6px; }
  #search { margin:0 0 8px; padding:8px 10px; font-size:16px; height:auto;
            border:1px solid #cfcfcf; border-radius:8px; box-shadow:none; }
  #search:focus { outline:none; border-color:#2b6cb0; box-shadow:0 0 0 3px #bee3f8; }
  .tbl-wrap { max-height:50vh; overflow-y:auto; background:#fff;
              border:1px solid #e2e2e2; border-radius:8px; }
  table.words { width:100%; border-collapse:collapse; table-layout:fixed; }
  table.words th:first-child, table.words td:first-child { width:70%; }
  table.words th:last-child,  table.words td:last-child  { width:30%; }
  table.words th { position:sticky; top:0; background:#f0f0f0; text-align:left;
                   padding:8px 10px; font-size:13px; }
  table.words td { padding:6px 10px; border-top:1px solid #eee; }
  table.words th:last-child, table.words td:last-child { text-align:center; }
  table.words th { cursor:pointer; user-select:none; }
  table.words th.asc::after  { content:' \\25B2'; font-size:10px; }
  table.words th.desc::after { content:' \\25BC'; font-size:10px; }
"

js <- "
  function solveBoard() {
    var cells = $('.cell').map(function() { return this.value; }).get();
    Shiny.setInputValue('board', cells, {priority: 'event'});
  }

  // Typing: keep one letter (Q becomes Qu), then jump to the next box
  $(document).on('input', '.cell', function() {
    var letters = this.value.replace(/[^A-Za-z]/g, '');
    if (!letters) { this.value = ''; return; }
    var ch = letters.slice(-1).toUpperCase();
    this.value = (ch === 'Q') ? 'Qu' : ch;
    var i = $('.cell').index(this);
    var next = $('.cell').eq(i + 1);
    if (next.length) { next.focus(); } else { $('#solve').focus(); }
  });

  $(document).on('keydown', '.cell', function(e) {
    var i = $('.cell').index(this);
    if (e.key === 'Backspace' && this.value === '' && i > 0) {
      e.preventDefault(); $('.cell').eq(i - 1).focus();
    } else if (e.key === 'ArrowLeft' && i > 0) {
      e.preventDefault(); $('.cell').eq(i - 1).focus();
    } else if (e.key === 'ArrowRight' && i < 15) {
      e.preventDefault(); $('.cell').eq(i + 1).focus();
    } else if (e.key === 'ArrowUp' && i >= 4) {
      e.preventDefault(); $('.cell').eq(i - 4).focus();
    } else if (e.key === 'ArrowDown' && i < 12) {
      e.preventDefault(); $('.cell').eq(i + 4).focus();
    } else if (e.key === 'Enter') {
      e.preventDefault(); solveBoard();
    }
  });

  // Select the tile's contents on focus so typing replaces it
  $(document).on('focus', '.cell', function() { this.select(); });
  $(document).on('mouseup', '.cell', function(e) { e.preventDefault(); });

  $(document).on('click', '#solve', solveBoard);
  $(document).on('click', '#clear', function() {
    $('.cell').val('');
    $('.cell').first().focus();
    Shiny.setInputValue('clear', Math.random(), {priority: 'event'});
  });

  // Segmented controls report their value to Shiny
  $(document).on('click', '.seg button', function() {
    var $b = $(this);
    $b.siblings().removeClass('on');
    $b.addClass('on');
    Shiny.setInputValue($b.closest('.seg').attr('id'), $b.data('value'));
  });

  // Click a column header to sort the results table (runs in the browser).
  // The chosen sort is re-applied whenever the table is redrawn by a filter.
  var sortState = null;

  function applySort() {
    if (!sortState) return;
    var $t = $('table.words');
    if (!$t.length) return;
    var col = sortState.col, dir = sortState.dir;
    var rows = $t.find('tbody tr').get();
    rows.sort(function(a, b) {
      var x = $(a).children().eq(col).text();
      var y = $(b).children().eq(col).text();
      var c;
      if (col === 1) {
        c = parseInt(x, 10) - parseInt(y, 10);
        if (c === 0) {
          c = $(a).children().eq(0).text().localeCompare($(b).children().eq(0).text());
        }
      } else {
        c = x.localeCompare(y);
      }
      return dir === 'asc' ? c : -c;
    });
    $t.find('tbody').append(rows);
    $t.find('th').removeClass('asc desc').eq(col).addClass(dir);
  }

  $(document).on('click', 'table.words th', function() {
    var col = $(this).index();
    if (sortState && sortState.col === col) {
      sortState.dir = (sortState.dir === 'asc') ? 'desc' : 'asc';
    } else {
      sortState = {col: col, dir: (col === 1 ? 'desc' : 'asc')};
    }
    applySort();
  });

  new MutationObserver(function(muts) {
    for (var i = 0; i < muts.length; i++) {
      for (var j = 0; j < muts[i].addedNodes.length; j++) {
        var n = muts[i].addedNodes[j];
        if (n.nodeType === 1 && (n.classList.contains('tbl-wrap') ||
            n.querySelector('.tbl-wrap'))) { applySort(); return; }
      }
    }
  }).observe(document.documentElement, {childList: true, subtree: true});

  $(document).on('shiny:connected', function() {
    $('.seg').each(function() {
      Shiny.setInputValue(this.id, $(this).find('button.on').data('value'));
    });
    $('.cell').first().focus();
  });
"


# ---- UI -----------------------------------------------------------------------

ui <- fluidPage(
  title = "Nathaniel's Thanksgiving Boggle Solver!",
  tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  
  div(
    class = "wrap",
    h1("Nathaniel's Thanksgiving", tags$br(), "Boggle Solver!"),
    
    div(
      class = "board",
      lapply(1:16, function(i) {
        tags$input(type = "text", class = "cell", maxlength = 3,
                   autocomplete = "off", autocapitalize = "characters",
                   autocorrect = "off", spellcheck = "false")
      })
    ),
    
    div(class = "msg", textOutput("msg")),
    
    div(
      class = "actions",
      tags$button(id = "solve", type = "button", class = "btn-main", "Find words"),
      tags$button(id = "clear", type = "button", class = "btn-quiet", "Clear")
    ),
    
    conditionalPanel(
      condition = "output.has_results",
      div(
        class = "controls",
        div(class = "ctl-label", "Shortest word"),
        seg("min_len", c("3 letters", "4 letters"), c(3, 4), selected = 4),
        div(class = "ctl-label", "Commonness"),
        seg("common", names(LEVELS), seq_along(LEVELS), selected = DEFAULT_LEVEL)
      ),
      div(class = "count", textOutput("count")),
      textInput("search", NULL, placeholder = "Search words...", width = "100%"),
      uiOutput("table")
    )
  )
)


# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  
  results <- reactiveVal(NULL)   # data frame of ALL words found for the board
  msg     <- reactiveVal("")
  
  # Solve only when the button (or Enter) is used
  observeEvent(input$board, {
    cells <- toupper(trimws(unlist(input$board)))
    
    if (length(cells) != 16 || any(!grepl("^[A-Z]+$", cells))) {
      msg("Fill in all 16 tiles first.")
      results(NULL)
      return()
    }
    
    cells[cells == "Q"] <- "QU"
    board <- matrix(cells, nrow = 4, byrow = TRUE)
    
    found <- solve_boggle(board, DATA$sets)
    df <- data.frame(word = found, stringsAsFactors = FALSE)
    df$rank <- DATA$freq$rank[match(df$word, DATA$freq$word)]
    
    msg("")
    results(df)
  })
  
  # Clear the results (and the search box) when the Clear button is pressed
  observeEvent(input$clear, {
    results(NULL)
    msg("")
    updateTextInput(session, "search", value = "")
  })
  
  # Filters are cheap and re-run instantly; they never re-solve the board
  filtered <- reactive({
    df <- results()
    req(df, input$min_len, input$common)
    
    df <- df[nchar(df$word) >= input$min_len, , drop = FALSE]
    
    cutoff <- LEVELS[[as.integer(input$common)]]
    if (is.finite(cutoff)) {
      df <- df[!is.na(df$rank) & df$rank <= cutoff, , drop = FALSE]
    }
    
    # Search box: match anywhere in the word, ignoring case
    q <- toupper(trimws(paste0(input$search, "")))
    if (nzchar(q)) {
      df <- df[grepl(q, df$word, fixed = TRUE), , drop = FALSE]
    }
    
    # Longest words first, alphabetical within each length
    df <- df[order(-nchar(df$word), df$word), , drop = FALSE]
    data.frame(Word = df$word, Letters = nchar(df$word), stringsAsFactors = FALSE)
  })
  
  output$has_results <- reactive(!is.null(results()))
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  
  output$msg <- renderText(msg())
  
  output$count <- renderText({
    n <- nrow(filtered())
    if (n == 0) "No words match. Try loosening the filters."
    else paste(n, if (n == 1) "word" else "words")
  })
  
  # Plain HTML table (no DT, so far fewer packages download in the browser)
  output$table <- renderUI({
    df <- filtered()
    req(nrow(df) > 0)
    div(class = "tbl-wrap",
        tags$table(class = "words",
                   tags$thead(tags$tr(tags$th("Word"), tags$th("Letters"))),
                   tags$tbody(lapply(seq_len(nrow(df)), function(i) {
                     tags$tr(tags$td(df$Word[i]), tags$td(df$Letters[i]))
                   }))
        )
    )
  })
}

shinyApp(ui, server)