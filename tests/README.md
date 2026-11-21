# Rhizome regression tests

Plain SBCL scripts that exercise window-management logic against
stubbed river requests. Run with:

    sbcl --non-interactive --load tests/workspace-restore.lisp
    sbcl --non-interactive --load tests/workspace-switch-away.lisp

- `workspace-restore.lisp` — switching back to a workspace must SHOW
  its previously hidden windows (regression: the show-gate was once
  inverted, hiding windows forever).
- `workspace-switch-away.lisp` — switching away must HIDE the old
  workspace's windows and SHOW the new one's, without redundant
  show/hide requests (which cause river manage/render loops).
- `window-rules.lisp` — app-id/title regex rules assign workspaces.
- `floating.lisp` — float/tile state, geometry clamps, interactive
  resize math, manage/render integration, and `:float` window rules.
- `float-centring.lisp` - where a new float first appears: centred on
  the frame holding its tiled parent window, on the active frame when
  the parent floats, else on its output's usable area; it follows a
  parent shown on another output; it is placed once its size is known
  and a later move is kept; a window already on screen floats where it
  is.
- `dialog-placement.lisp` - dialogs: a window with a parent floats on
  map unless `(:dialogs (:float nil))` or a matching rule with
  `:float nil` says otherwise, once per window, or when its parent
  arrives within two seconds of map; it goes to the active
  frame when the parent floats, is minimized, has no frame, or has a
  frame smaller than the dialog in either dimension; else to the
  parent's frame, with a `group:screen:frame` notice (length and colour
  from `:dialogs`) only when that workspace is hidden; a dialog that
  would overhang its output moves back onto the usable area;
  `goto-last-dialog` (command, export, C-t d) shows and focuses the
  last dialog that opened on a hidden workspace, and with none, or
  once it has closed, changes nothing and shows a message.
- `focus-on-close.lisp` - focus recovery when a window closes: the next
  window in its frame (or the next column of a scroll ribbon), else the
  newest window on its workspace, else a cleared seat; focus and clear
  requests reach river only in the manage pass; unfocused closes and
  windows on other workspaces or outputs leave focus alone.
- `focus-after-capture.lisp` - keyboard focus returns to each seat's
  focused window when the prefix capture, a prompt, the eval prompt, a
  context menu, or the launcher closes; focus state does not change, and
  a missing or closed window gets nothing.
- `layer-focus.lisp` - layer shell focus events delivered as wayflan
  reads them: focus_none gives each seat's focused window the keyboard
  back in the next manage pass without changing focus state;
  focus_exclusive and focus_non_exclusive are logged and queue nothing;
  a missing or closed window gets nothing.
- `frames.lisp` — frame tree operations: splits, merges, resizes,
  directional focus, cycling, tree construction from rectangles,
  layout templates, and manage/render integration.
- `frame-pruning.lisp` - empty-frame pruning: frames a user split makes
  survive manage passes and the move of their last window; frames a new
  window split open still close with it; `(:frames (:prune-empty :never))`
  keeps every empty frame and `:always` prunes user splits too; removing a
  split or collapsing to one frame leaves nothing marked.
- `frame-adoption.lisp` - every tiled window stays in a leaf of its
  workspace's tree: the manage pass that splits a frame for a new window
  proposes both halves their size, and a window whose frame left the tree
  (a layout switch while minimized, removing a split, expose) is adopted
  again with a log line; a new window never follows a stale frame.
- `unfloat-placement.lisp` - where a float lands when it returns to the
  tiling, on its own output: the empty frame under its centre, else the
  empty frame nearest its centre, else a new half of the largest frame.
  Ties go to the frame nearest the centre, then to tree order; a float
  with no known size takes the first frame in tree order.
- `placement-policy.lisp` - frame placement policy: by default a split
  fills its new half with the most recently focused hidden window and a
  new window enters the focused frame; `:new-frame :empty`, the
  `:new-window` preferences (`:empty`, `:last`, `:unfocused`, functions),
  a custom policy class selected by name, the fallback for an unknown
  name, and the workspace focus history.
- `split-focus.lisp` - the current frame after a split: it stays on
  the original half, with or without a focused window, so a second
  split divides that half again; the placement policy still fills the
  new half.
- `grow-frame.lisp` - grow-frame and grow-frame-to-edge: a frame grows
  into the neighbour that shares its whole edge, sibling or not, or to
  the screen edge across every frame in the way; absorbed windows queue
  behind the shown window and none is lost. A frame over 3/4 of the
  axis grows to the edge in one press, falling back to one step when
  that is refused; exactly 3/4 takes one step. A frame spanning the whole
  screen on the growth axis splits in half, its shown window taking the
  half on the growth side and the other half keeping the queue. A split
  neighbour, a taller or wider neighbour, a screen edge the frame only
  touches, an empty frame with nothing to move, a frame in the way that
  runs past the frame's sides, and a result no split tree can hold are
  each refused with a reason and an unchanged tree.
- `parity.lisp` — Alt+drag pointer bindings (drag floats tiled
  windows), the C-t h help system, describe-key, move-window-to-frame,
  and `:output` window rules.
- `quickwins.lisp` — StumpWM quick-wins: window numbering (1..9, 0,
  overflow, reuse, renumber), pull/exchange/only/balance/fclear,
  lastmsg/time, quit-confirm, fullscreen request queueing, F-keys.
- `client-fullscreen.lisp` - client fullscreen and exit-fullscreen
  requests queue the same manage-pass request as C-t F11; the
  requested output is used when river still reports it, otherwise
  the window's own output.
- `interactivity.lisp` — multi-line messages, which-key hints,
  iresize (arrows resize until Enter/Esc), and command mode (sticky
  prefix: multiple commands until Escape).
- `medium3.lisp` — frame dump/restore round-trips (shape, rects,
  window placement, output scaling, missing/unmentioned windows),
  window marks lifecycle and move-marked, and set-prefix-key's key
  spec parsing and binding swaps.
- `bigger1.lisp` — minor modes (precedence over the prefix map,
  stacking order, enable/disable), WSE expressions (and/or/not/any,
  float/marked/number selectors, :wse window rules), and fuzzy
  matching (subsequence + substring-first ranking).
- `scroll.lisp` — the niri-style ribbon: enter/exit transitions
  (frame order becomes column order, dynamic re-adoption on exit),
  column geometry, viewport clamping, focus-follows-visibility,
  close-pruning with focus recovery, arrivals next to focus, floats
  leaving the ribbon, toggling, and pipeline dispatch routing.
- `dbus.lisp` — the pure-CL DBus wire layer: marshalling roundtrips
  for every wire type (including the header a(yv) array), then LIVE
  integration against the real session bus: SASL auth, Hello,
  ListNames, RequestName/GetNameOwner round-trip, Properties.Get on
  the broker, and the reader thread.
- `expose.lisp` — the C-t Return window overview: grid geometry
  (aspect-fit, bar offset), layout round-trip (floats, frame
  membership, per-frame order, focus), digit/arrow/Return/click
  picking, and refusal with too few windows.
- `bar-guard.lisp` — the bar fork-bomb regression: the bar process
  (*bar-process-mode*) must never spawn or stop bars, no matter what
  the user config asks for; bar-apply-config / toggle-bar / start /
  stop all refuse there, and start/stop-bar now reap orphan bars.
- `bar-hide.lisp` - toggle-bar hides and shows the running bar
  process instead of restarting it, and the bar reads :bar-hidden from
  the WM snapshot. The bar's space is reserved only while river reports
  an exclusive zone at least as tall as the bar. A configure that
  leaves the :bar settings alone keeps the bar, and two concurrent
  starts launch one process.
- `bar-per-output.lisp` - one bar per output: the WM snapshot carries
  each output's workspace, layout, window count, occupancy, and bar
  flags; `(:bar :outputs ...)` picks the outputs and the primary bar;
  each output reserves the strip only when it carries a bar. The bar
  process keeps one surface per wl_output, follows outputs added and
  removed, survives the compositor closing one bar, renders each
  output's own view, keeps the tray on the primary bar, and routes a
  workspace click to its own output.
- `session-icons.lisp` — session management (lock command building,
  swayidle arming, logout confirm flow) and the bar icons module
  (icon discovery, 16x16 scaling, caching, per-entry actions).
- `repl-server-config.lisp`: how the REPL server settings resolve.
  `:slynk` is read, an older `:swank` entry is honoured while `:slynk` is left at its
  defaults, `:slynk` wins when both are set, and the defaults apply.
- `repl-server-port.lisp`: `repl-server-port` reports the port once the server
  listens, and a second `start-slynk` starts no second server. Server creation
  is stubbed; no slynk server is started.
- `hooks-and-eval.lisp` — hook firing/removal/error isolation and the
  eval prompt's evaluator.
- `keymap-layers.lisp` - keymap objects (parents, shadowing,
  enumeration) and the user/default layering: user bindings win over
  defaults whether made before or after them, and survive repeated
  manage sequences, set-user-mod, and set-prefix-key; reload-config
  replaces the user layer; define-key, define-prefix-key and
  define-keymap-key keep working.
- `key-sequences.lisp` - kbd parsing (C M s H A S, sequences), a
  direct key bound to a keymap (s-x then f), modifiers on later keys
  of a sequence (s-x C-M-f), the prefix key as an ordinary binding
  (C-t, C-', set-prefix-key to s-t), zero-argument and event-taking
  bindings with their specials, sticky keymaps, and grabs skipped for
  an unmapped modifier until set-modifier maps it.
- `key-grabs.lisp` - grab sync against stubbed river requests: river
  grabs exactly the first keys the top keymaps bind; binding,
  unbinding, undefine-key, set-user-mod, set-modifier, set-prefix-key,
  clearing the user keymaps, and seats coming and going move only the
  grabs that change; a sync with nothing changed sends no requests; an
  undefined key inside a sequence runs nothing.
- `key-names.lisp` - key names and keysym printing: kbd reads
  ISO_Left_Tab and the brightness, volume, and mute keys,
  key-description prints them back, and a keysym prints as its name,
  as its character (Latin-1, legacy, and Unicode keysyms), or as #N,
  in which-key text and the key-pressed log, while its binding still
  runs.
- `multi-head.lisp` - two side-by-side outputs: which output is
  current (focus, last selection, pointer), new windows opening on
  it, focusing the next/previous/left/right output (an empty one
  clears focus), moving a window across outputs (frame left behind
  and pruned, adoption on the target, float offsets), and IPC
  commands acting on the current output.
- `ipc.lisp` — command dispatch and a real unix-socket round-trip
  (server thread + client thread + main-thread queue drain).
- `workspace-focus.lisp` - keyboard focus across workspace changes:
  switching to an empty workspace clears focus; switching back focuses
  the window last focused there, or the newest unminimized window
  there; moving the focused marked window off the visible workspace
  hands focus to a window that stays; and when the focused window is
  moved, the view follows the window and it keeps focus.
- `prompt-free-text.lisp`: Enter on a prompt opened without candidates
  completes with the typed text (and with nothing when it is empty),
  Enter on a candidate prompt still needs a matching candidate, and the
  shell prompt launches the command typed into it.
- `prompt-popup.lisp` - how a candidate prompt draws: its height holds
  whole rows, no more than it has candidates; the selected row is a
  highlight the full height of the row, inside the popup at every
  scroll position; and the background is opaque whatever the bar's
  opacity.
- `palette-message.lisp` - the message a command leaves on screen when
  run from the command palette or over IPC: the command's own message
  stays, a non-empty reply is shown when the command showed nothing,
  and an empty reply never replaces a message.
- `command-table.lisp` - a command defined from another package
  through the exported API runs through execute-command, the IPC
  dispatcher, and the command palette, and describe-command shows its
  docstring; redefining it replaces the handler and the docstring.
- `workspace-numbering.lisp` - workspace numbers, counts, and names:
  the display base (0 or 1) at the keys, IPC, the picker, window
  rules, and `:types`; per-screen counts and names keyed by connector
  name; a smaller configured count moving windows to the screen's
  last workspace; add-workspace and remove-workspace; reload-config
  restoring the configured count; and the bar's per-screen cells and
  clicks.
- `shm-buffers.lisp` - layer-surface shm buffers: repeated configures
  of one surface hold one shm file, and tearing down the bar, the
  message window, the prefix capture surface, the prompt or the eval
  prompt leaves none; a second teardown is harmless. Covers the
  compositor closing each overlay, a second message arriving before
  the first is configured or during a resize (one surface, one
  buffer), and reopening an overlay over a state that still holds a
  buffer.
- `pull-hidden.lisp` - pull-hidden-next and its previous direction:
  hidden windows cycle in window-number order whatever order the
  workspace lists them in, wrapping at either end; unnumbered windows
  come after the numbered ones.
- `scratchpads.lisp` - the scratchpad module and the minimize paths:
  hiding a scratchpad, a client's minimize request, and the window
  menu's Minimize each send one river hide in the next render pass;
  showing a scratchpad and the menu's Restore each send one show;
  reloading the module adds no second hook handler, and launching a
  scratchpad and claiming its window leave the `:window-created`
  handlers as they were; a scratchpad centres on its own output's
  usable area (or the current output while it has none yet), 50
  pixels below that output's bar; a scratchpad claimed before its
  client commits a size centres again on that first size, and only
  that one; every show moves a scratchpad's decorated frame (titlebar
  and border included) back inside its output's usable area, and one
  already inside keeps its position; a frame that fits keeps its size,
  and one larger than the area shrinks to fit with one log line; hiding a
  scratchpad, a minimize request, and the menu's Minimize each move
  the focus to a visible window on the workspace, or clear it when
  none is left, and showing a scratchpad focuses it; after a minimize
  request or the menu's Minimize one toggle shows the scratchpad, and
  after the menu's Restore, the menu's Move to workspace, or a move to
  another workspace or output one toggle hides it; each way of
  restoring a minimized window runs `:window-restored` once, and
  moving a window that is not minimized runs it not at all.
- `display-layout.lisp` - the `(:displays (:layout ...))` output
  layout: relative placement against heads of different sizes,
  normalization to a 0,0 origin, explicit positions, mode and scale
  overrides, matching by connector name or description, heads named
  but absent, heads present but not named, the no-op when the layout
  already holds, the log lines, and the automatic apply on the output
  manager's done event.
- `server-side-decorations.lisp`: decoration_hint handling, use_ssd
  and use_csd sent only in the manage pass, the
  `(:windows :decorations)` key, and set_tiled edges for tiled windows.
- `display-brightness.lisp` - display menu brightness argv: signed
  steps use brightnessctl's relative N%+ / N%- syntax, absolute levels
  plain N%.
- `frame-outline.lisp` - the focused-frame outline: which frame and
  output carry it (empty or occupied frames, after frame focus moves,
  splits and removals), when it hides (fullscreen, scroll and floating
  workspaces, switched off), the theme settings, the ring pixels, and
  the shell surface lifecycle (one per output, redraw only on change,
  destroyed with its output), and the empty-frame colour and width:
  default, configured, `nil` falling back to the outline colour or
  width, and the redraw when a window arrives in or leaves the focused
  frame.
- `titlebars.lisp` - float titlebars: which windows carry one (a plain
  float does; a tiled window, a window with a parent, a fixed-size
  window, a CSD-only window, a window whose rule sets `:titlebar nil`,
  and a fullscreen, maximized or minimized float do not, nor any window
  when titlebars are off or decorations are left to clients), the
  strip's height, geometry, colours and title fitting, redraws only when
  the title, size, focus or colours change, a new buffer on any size
  change, teardown when the window tiles, goes fullscreen or closes,
  a press on the strip starting a move, and a float with a strip kept
  below the bar.
- `fselect.lisp` - frame selection by label (C-t f): which frames
  carry a label and in what order across outputs (empty and
  minimized-only frames skipped, floating workspaces skipped), the key
  to frame mapping, cancel keys, the pick moving focus to another
  output's frame, the C-t f and C-t F bindings, the selection timeout
  (20 s default, a configured value, NIL arming none), each label's
  corner away from the message window, the hint echoed in the bar and
  kept out of the message window, and the label surfaces torn down on
  a pick, C-g, Escape, any other key, the timeout, and the removal of
  an output.
- `focus-empty-frames.lisp` - focus into empty frames and frame
  launches: `(:frames (:focus-empty ...))` off and on, toggled at
  runtime, for the C-t arrows and C-t f; keyboard focus cleared and the
  next new window landing in the empty frame; `focus-frame`;
  `launch-into-frame` into a frame on another output, an occupied
  frame, and a hidden workspace, which stays hidden; claims matched by
  process ancestry, lapsed claims, and windows older than the launch.
- `bindings-overridable.lisp` - every default key binding, enumerated
  from the default keymaps, rebound, unbound, and moved to a new key from
  a user init, whether the init loads before the defaults exist (WM
  start) or runs after them. The user's choice, and every other
  default, holds through manage sequences, set-user-mod, set-prefix-key,
  a rebuild of the defaults, reload-config, an output hotplug, and
  configure. Also covers swapping C-t f and C-t F, a user key inside a
  default sub-keymap (C-t h, C-t M, C-t e), and an unbind on a literal
  modifier across set-user-mod.
- `modules.lisp` - load-module: only .asd-carrying directories in
  the module directory are registered with ASDF, an unknown module
  fails quietly, the first load loads the system, the second is an
  idempotent no-op, and the registry never duplicates an entry.
- `bar-adoption.lisp` - a WM restart adopts a surviving bar instead
  of cold-starting a fresh one: pgrep parsing, adoption in start-bar
  (extras reaped, nothing launched), adopted liveness in bar-alive-p,
  and the handover flag. All subprocess calls stubbed.
- `ipc-command-args.lisp` - IPC command lines reach their handlers
  with each argument parsed once: select-window, pull, move-marked,
  and resize-split take an integer argument over IPC or a string one
  from execute-command; wse and putsel take the rest of the line,
  spaces included; scratchpad with no argument keeps the default
  scratchpad. Downstream calls stubbed.
- `layout-rule-frames.lisp` - a window rule's :layout: the rule's
  workspace stores the layout and its frame tree is rebuilt from the
  layout's template, with the matching window in it even before it
  has an output; the output's layout mirror follows only the
  showing workspace; a repeated match keeps the frames; an unknown
  layout changes nothing.
- `ipc-socket-path.lisp` - the WM and session/rhizome-cmd name the
  same socket: $XDG_RUNTIME_DIR/rhizome.sock, or /tmp/rhizome-UID.sock
  when XDG_RUNTIME_DIR is unset or empty. The client's path is read
  from its "no socket" message; no socket is opened.
- `example-config.lisp` - examples/config.lisp loads without
  signalling, the values it marks as defaults are the defaults, and
  every `rhizome-cmd` command it names exists.
- `network-menu.lisp` - the network configurator (a MODULE: the
  suite loads rhizome-net from the repo's modules/ directory the
  way a user loads it): nmcli terse-line
  parsing (escaped colons in SSIDs), scan dedupe by SSID (strongest
  band wins) and signal sort, signal bars, connect/toggle argv.
  All nmcli calls stubbed.
- `bar-reserved-per-output.lisp` - the layout reserves bar space per
  output: zone reported on A reserves on A, hidden on B reserves
  nothing on B, no reports fall back to config, and the no-argument
  form keeps any-output semantics.
- `bar-system-modules.lisp` - the `:hostname`, `:memory`, and `:cpu`
  bar modules: `/proc/meminfo` and `/proc/stat` parsing against
  captures recorded on a real host (`fixtures/proc`), the CPU
  percentage from the change between two samples, the `MEM --` and
  `CPU --` placeholders for a missing, unreadable, or malformed file,
  the memoized host name, and registration by name.
- `notifications.lisp` - the desktop notification server: ids
  increase and `replaces_id` keeps its id and becomes the newest;
  markup is stripped; -1 takes `(:notifications :timeout-seconds)`, 0
  never expires; NotificationClosed carries reason 1 on expiry, 2 on
  dismiss, and 3 on CloseNotification; the window manager gets one
  line per change carrying the summary and body lines, and its
  notification commands return NIL or a string. Method calls get an
  error reply for a wrong path, interface, method, or signature, and
  for a handler that fails. Malformed, big-endian, and oversized
  messages on a socket pair are dropped while the reader keeps reading,
  and a header past the protocol maximum ends the read. The window
  manager's request parser interns nothing and refuses unknown keys
  and over-long requests; long lines wrap to the message window; a
  notification with no expiry has no deadline. The bus section runs
  on a private `dbus-daemon` with no activatable services, started for
  the run: a name another client owns leaves the server off with one log line, then Notify, GetCapabilities,
  GetServerInformation, Introspect, CloseNotification, Dismiss, and
  expiry over the bus. That section is skipped when `dbus-daemon` is
  not installed.
- `guest-start.sh` - a shell test, run with `sh tests/guest-start.sh`
  and not one of the Lisp suites: with a stub sbcl and a stub
  river-init, session/guest-start names each thing a guest lacks
  (Quicklisp, `~/.sbclrc`, a source registry entry for the share, an
  sbcl that starts), exits without starting the window manager when
  one is missing, and otherwise logs the rhizome source tree and hands
  over to river-init.
