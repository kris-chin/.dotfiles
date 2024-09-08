#Requires AutoHotkey v2.0.2
#SingleInstance Force

; Config from https://lgug2z.github.io/komorebi/common-workflows/autohotkey.html

;Define Komorebic command for easier access
Komorebic(cmd) {
    RunWait(format("komorebic.exe {}", cmd), , "Hide")
}

;;start and stop komorebi (ctrl + shft + win)
^+#]::Komorebic("start") ;;explicitly enable ffm for focus_folows_mouse
^+#[::Komorebic("stop") ;;TODO: grab all windows and move them back to the front

;;toggle tiling for workspace (win + [)
#[::Komorebic("toggle-tiling")

;;Focus windows (ctrl + win + hjkl)
^#h::Komorebic("focus left")
^#j::Komorebic("focus down")
^#k::Komorebic("focus up")
^#l::Komorebic("focus right")

;;Move windows (shift + win + hjkl)
+#h::Komorebic("move left")
+#j::Komorebic("move down")
+#k::Komorebic("move up")
+#l::Komorebic("move right")

;;Resize Windows (ctrl + shift + win + hjkl)
^+#h::Komorebic("resize-axis horizontal decrease")
^+#j::Komorebic("resize-axis vertical decrease")
^+#k::Komorebic("resize-axis vertical increase")
^+#l::Komorebic("resize-axis horizontal increase")

;;Maximize (win + f)
#f::Komorebic("toggle-maximize")


; Workspaces
#1::Komorebic("focus-workspace 0")
#2::Komorebic("focus-workspace 1")
#3::Komorebic("focus-workspace 2")
#4::Komorebic("focus-workspace 3")
#5::Komorebic("focus-workspace 4")
#6::Komorebic("focus-workspace 5")
#7::Komorebic("focus-workspace 6")
#8::Komorebic("focus-workspace 7")

; Move windows across workspaces
#+1::Komorebic("move-to-workspace 0")
#+2::Komorebic("move-to-workspace 1")
#+3::Komorebic("move-to-workspace 2")
#+4::Komorebic("move-to-workspace 3")
#+5::Komorebic("move-to-workspace 4")
#+6::Komorebic("move-to-workspace 5")
#+7::Komorebic("move-to-workspace 6")
#+8::Komorebic("move-to-workspace 7")

; Manipulate windows
#o::Komorebic("toggle-float")
; This one solos the window, but I don't know why I need this when I have fullscreen
; #i::Komorebic("toggle-monocle")

#+q::Komorebic("close")
#+a::Komorebic("minimize")
