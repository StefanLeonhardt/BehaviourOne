# Dies ist eine Shiny web Applikation zum Tagging von Judowettkämpfen #

# Setup ####
## Set maximum upload Größe auf 5GB pro Video ####
options(shiny.maxRequestSize = 5000 * 1024^2)
## R-Pakete laden ####
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(base64enc)

## Daten laden ####

# Erstelle Vektoren für Kategoriensystem
griffposition_T <- c("","kein Griff","Ärmel-Revers", "Ärmel-Rücken", "Ärmel-Kragen-oben", "Ärmel-Kragen-unten", 
                     "Ärmel-Hüfte", "Ärmel-Schulter", "Ärmel-Gürtel", "Ärmel-Ärmel", "Ärmel-NULL",
                     "Revers-Revers", "Revers-Ärmel", "Revers-Rücken", "Revers-Schulter", "Revers-Hüfte",
                     "Revers-Gürtel", "Revers-NULL", "Revers-Kragen-oben", "Revers-Kragen-unten",
                     "Gürtel-Gürtel", "Gürtel-Revers", "Gürtel-Rücken", "Gürtel-Hüfte", "Gürtel-Schulter",
                     "Gürtel-Kragen-oben", "Schulter-Schulter", "Schulter-Revers", "Schulter-Hüfte",
                     "Schulter-Rücken", "Schulter-Kragen-oben", "Schulter-Ärmel", "Schulter-Gürtel",
                     "CR-Ärmel-Rücken", "CR-Ärmel-Gürtel", "CR-Ärmel-Ärmel", "CR-Gürtel-Rücken",
                     "CR-Schulter-Rücken", "CR-Revers-Rücken", "CR-Revers-Gürtel", "CR-Revers-Ärmel",
                     "NULL-Revers", "NULL-Ärmel", "NULL-Rücken", "NULL-Schulter", "NULL-Kragen-oben",
                     "NULL-Kragen-unten", "NULL-Gürtel", "NULL-Hüfte", "DI-Ärmel-Revers", "DI-Ärmel-Gürtel",
                     "DI-Ärmel-Hüfte", "DI-Ärmel-Schulter", "DI-Ärmel-Ärmel", "DI-Ärmel-NULL",
                     "DI-Ärmel-Kragen", "DI-Revers-Rücken", "DI-Revers-Kragen", "DI-Revers-Hüfte",
                     "DI-Revers-NULL", "DI-Revers-Gürtel", "DI-Revers-Ärmel", "DI-Revers-Revers",
                     "ER-Ärmel-Revers", "ER-NULL-Revers", "ER-Revers-Revers", "ER-Ärmel-Ärmel",
                     "ER-Gürtel-Revers", "Hüfte-Kragen-oben", "Hüfte-Rücken", "Hüfte-Hüfte",
                     "Hüfte-Schulter", "Hüfte-Ärmel", "Lösen mit einer Hand", "Lösen mit zwei Händen", "Wegreißen")

nage_waza <- c("","Ashi-guruma", "Daki-wakare", "De-ashi-harai", "Hane-goshi", "Hane-goshi-gaeshi",
               "Hane-makikomi", "Harai-goshi", "Harai-goshi-gaeshi", "Harai-makikomi", "Harai-tsurikomi-ashi",
               "Hikikomi-gaeshi", "Hiza-guruma", "Ippon-seoi-nage", "Kata-Guruma", "Kawazu-gake",
               "Kibishu-gaeshi", "Koshi-guruma", "Ko-soto-gake", "Ko-soto-gari", "Ko-uchi-gaeshi",
               "Ko-uchi-gari", "Ko-uchi-makikomi", "Kuchiki-taoshi", "Morote-gari", "Obi-otoshi",
               "Obi-tori-gaeshi", "O-goshi", "O-guruma", "Okuri-ashi-harai", "O-soto-gaeshi",
               "O-soto-gari", "O-soto-guruma", "O-soto-makikomi", "O-soto-otoshi", "O-uchi-gaeshi",
               "O-uchi-gari", "Sasae-tsurikomi-ashi", "Seoi-nage", "Seoi-otoshi", "Sode-tsurikomi-goshi",
               "Soto-makikomi", "Sukui-nage", "Sumi-gaeshi", "Sumi-otoshi", "Tai-otoshi",
               "Tani-otoshi", "Tawara-gaeshi", "Tomoe-nage", "Tsubame-gaeshi", "Tsuri-goshi",
               "Tsurikomi-goshi", "Uchi-makikomi", "Uchi-mata", "Uchi-mata-gaeshi", "Uchi-mata-makikomi",
               "Uchi-mata-sukashi", "Uki-goshi", "Uki-otoshi", "Uki-waza", "Ura-nage",
               "Ushiro-goshi", "Utsuri-goshi", "Yama-arashi", "Yoko-gake", "Yoko-guruma",
               "Yoko-otoshi", "Yoko-wakare")

ne_waza <- c("","Ashi-garami", "Ashi-gatame", "Do-jime", "Gyaku-juji-jime", "Hadaka-jime",
             "Hara-gatame", "Hiza-gatame", "Juji-gatame", "Kami-shiho-gatame", "Kata-gatame",
             "Kataha-jime", "Kata-juji-jime", "Katate-jime", "Kesa-gatame", "Kuzure-kami-shiho-gatame",
             "Kuzure-kesa-gatame", "Nami-juji-jime", "Okuri-eri-jime", "Ryote-jime", "Sankaku-gatame",
             "Sankaku-jime", "Sode-guruma-jime", "Tate-shiho-gatame", "Te-gatame", "Tsukkomi-jime",
             "Ude-garami", "Ude-gatame", "Uki-gatame", "Ura-gatame", "Ushiro-kesa-gatame",
             "Waki-gatame", "Yoko-shiho-gatame")

griffposition_N <- c("Arm", "Armpit", "Belt", "Cest", "Hairikata", "Kata gatame hairikata",
                     "Leg", "Nek", "Pass the legs", "Sankaku hook", "Stomach")

angriffsposition <- c("Back", "Direct", "Down", "Front", "Inside", "Side DX", "Side SX", "Top")

taisabaki <- c("Shinzentai" = "SHT", "Jigotai" = "JGT","Ayumi-ashi" = "aya", "Mae-mawari-sabaki" = "mms", "Mae-sabaki" = "mas", "Mawari-komi" = "kaw", "Oi-komi" = "okm",
               "Tobi-komi" = "tob", "Tsugi-ashi" = "tas", "Ushiro-mawari-sabaki" = "ums", "Ushiro-sabaki" = "uss")

aktionsart <- c("Direktangriff" = "DA", "Konterangriff" = "KA", "Kombination" = "KO", "Finte" = "FI", "Ausweichen" = "AW", "Blocken" = "BL","Ausweichen mit Block" = "AWBL", "Übertriebenes Ausweichen" = "NO", "Blocken mit Ausweichen" = "BLAW", "keine Reaktion" = "kR")

bestrafung <- c("","Non-combativity", "False attack", "Not taking grip","Blocking with one hand", "Blocking with two hand", "Cross-gripping", "Covering lapel", "Stepping out", "Leg gripping", "Reverse Seoi-nage", "Gripping inside sleeve", "Not fixing Judogi", "Pistol Grip", "other Penalty")


## Definiere das Kategoriensystem mit Inputtypen ####
categories <- list(
  "Blau" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = list(
        type = "radio",
        choices = c("Seitlich zum Mattenrand" = "SMR", "Rückwärts zum Mattenrand" = "RMR", "Vorwärts zum Mattenrand" = "VMR", "Mattenzentrum" = "MZ", "Seitlich zur Mattenecke" = "SME", "Rückwärts zur Mattenecke" = "SME", "Vorwärts zur Mattenecke" = "VME")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("Links vs. Links" = "LL", "Rechts vs. Links" = "RL", "Links vs. Rechts" = "LR", "Rechts vs. Rechts" = "RR")
      ),
      "Art der Kontaktaufnahme" = list(
        type = "radio",
        choices = c("Überfallartig" = "UFA", "Offensiv" = "OFF", "Defensiv" = "DEF", "Neutral" = "NEU")
      )
    ),
    "Kumi kata" = list(
      "Griffsystem" = list(
        type = "select",
        choices = griffposition_T
      ),
      "Distanz" = list(
        type = "radio",
        choices = c("Halbdistanz" = "LAN", "Infight" = "KUR")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("Links vs. Links" = "LL", "Rechts vs. Links" = "RL", "Links vs. Rechts" = "LR", "Rechts vs. Rechts" = "RR")
      )      
    ),
    "Aktion Stand" = list(
      "Art der Aktion" = list(
        type = "select",
        choices = aktionsart
      ),
      "Nage waza" = list(
        type = "selectize",
        choices = nage_waza
      ),
      "Wertung" = list(
        type = "radio",
        choices = c("","No Score" = "NS", "Yuko" = "YU", "Waza-ari" = "WA", "Ippon" = "IP")
      ),
      "Taisabaki" = list(
        type = "radio",
        choices = taisabaki
      )
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = list(
        type = "radio",
        choices = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion")
      ),
      "Kontaktaufnahme" = list(
        type = "radio",
        choices = angriffsposition
      )
    ),
    "Aktion Boden" = list(
      "Ne waza" = list(
        type = "selectize",
        choices = ne_waza
      ),
      "Griffbeginn" = list(
        type = "radio",
        choices = griffposition_N
      )
    ),
    "Unterbrechung" = list(
      "Wertung" = list(
        type = "radio",
        choices = c("", "Shido 1" = "S1", "Shido 2" = "S2", "Shido 3" = "S3", "Hansokumake" = "HAN")
      ),
      "Art der Bestrafung" = list(
        type = "selectize",
        choices = bestrafung
      ),
      "weitere Gründe" = list(
        type = "radio",
        choices = c("", "Verletzung ohne Mattenarzt" = "VoM","Verletzung mit Mattenarzt" = "VmM", "Kiken gachi", "Sonstiges")
      )
    )
  ),
  "Weiss" = list(
    "Kontaktaufnahme" = list(
      "Laufrichtung" = list(
        type = "radio",
        choices = c("Seitlich zum Mattenrand" = "SMR", "Rückwärts zum Mattenrand" = "RMR", "Vorwärts zum Mattenrand" = "VMR", "Mattenzentrum" = "MZ", "Seitlich zur Mattenecke" = "SME", "Rückwärts zur Mattenecke" = "SME", "Vorwärts zur Mattenecke" = "VME")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("Links vs. Links" = "LL", "Rechts vs. Links" = "RL", "Links vs. Rechts" = "LR", "Rechts vs. Rechts" = "RR")
      ),
      "Art der Kontaktaufnahme" = list(
        type = "radio",
        choices = c("Überfallartig" = "UFA", "Offensiv" = "OFF", "Defensiv" = "DEF", "Neutral" = "NEU")
      )
    ),
    "Kumi kata" = list(
      "Griffsystem" = list(
        type = "select",
        choices = griffposition_T
      ),
      "Distanz" = list(
        type = "radio",
        choices = c("Halbdistanz" = "LAN", "Infight" = "KUR")
      ),
      "Kampfauslage" = list(
        type = "radio",
        choices = c("Links vs. Links" = "LL", "Rechts vs. Links" = "RL", "Links vs. Rechts" = "LR", "Rechts vs. Rechts" = "RR")
      )      
    ),
    "Aktion Stand" = list(
      "Art der Aktion" = list(
        type = "select",
        choices = aktionsart
      ),
      "Nage waza" = list(
        type = "selectize",
        choices = nage_waza
      ),
      "Wertung" = list(
        type = "radio",
        choices = c("","No Score" = "NS", "Yuko" = "YU", "Waza-ari" = "WA", "Ippon" = "IP")
      ),
      "Taisabaki" = list(
        type = "radio",
        choices = taisabaki
      )
    ),
    "Übergang-Stand-Boden" = list(
      "Art des ÜSB" = list(
        type = "radio",
        choices = c("nach gegner. Angriff", "nach eigen. Angriff", "nach gegner. Aktion", "nach eigen. Aktion")
      ),
      "Kontaktaufnahme" = list(
        type = "radio",
        choices = angriffsposition
      )
    ),
    "Aktion Boden" = list(
      "Ne waza" = list(
        type = "selectize",
        choices = ne_waza
      ),
      "Griffbeginn" = list(
        type = "radio",
        choices = griffposition_N
      )
    ),
    "Unterbrechung" = list(
      "Wertung" = list(
        type = "radio",
        choices = c("", "Shido 1" = "S1", "Shido 2" = "S2", "Shido 3" = "S3", "Hansokumake" = "HAN")
      ),
      "Art der Bestrafung" = list(
        type = "selectize",
        choices = bestrafung
      ),
      "weitere Gründe" = list(
        type = "radio",
        choices = c("", "Verletzung ohne Mattenarzt" = "VoM","Verletzung mit Mattenarzt" = "VmM", "Kiken gachi", "Sonstiges")
      )
    )
  )
)

# Füge Abkürzungs-Aliase
categories[["B"]] <- categories[["Blau"]]
categories[["W"]] <- categories[["Weiss"]]

# Phase-Abkürzungen definieren
phase_abbreviations <- c(
  "Kontaktaufnahme" = "KA",
  "Kumi kata" = "KK", 
  "Angriff Stand" = "AS",
  "Übergang-Stand-Boden" = "ÜSB",
  "Angriff Boden" = "AB"
)

# User Interface für Shiny app ####
ui <- page_navbar(
  title = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "icon_judo.png",  # Datei im www-Ordner
      height = "45px",
      style = "margin-right: 10px;"  # Abstand NACH rechts
    ),
    "Behaviour One"
  ),
  theme = bs_theme(version = 5,
                   bootswatch = "minty"),
  nav_spacer(),
  nav_item(
    input_dark_mode(
      id = "dark_mode",
      mode = "dark"
    )
  ),

  header = tags$head(
      # Lade Font Awesome für Icons
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$style(HTML('
  .video-container {
    position: relative;
    width: 100%;
    padding-top: 56.25%;
  }
  .video-container video {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
  }
  /* Hover-Effekt für klickbare Zeilen */
  .clickable-row { cursor: pointer; }
  .clickable-row:hover { background-color: #f0f8ff !important; }

  /* Zeit-Wert formatieren */
  .time-display {
    font-family: monospace;
    color: #0066cc;
    text-decoration: underline;
    cursor: pointer;
    font-weight: bold;
  }

  .time-display:hover {
    color: #004499;
    background-color: #f0f8ff;
  }

  /* Lösch-Button */
  .delete-btn {
    color: white;
    cursor: pointer;
    background-color: #e26c78ff;
    border: none;
    padding: 5px 10px;
    border-radius: 4px;
  }
  .delete-btn:hover {
    color: white;
    background-color: #e26c78ff;
  }

  /* Edit-Button */
  .edit-btn {
    color: white;
    cursor: pointer;
    background-color: #6bdfeeff;
    border: none;
    padding: 5px 10px;
    border-radius: 4px;
    margin-right: 5px;
  }
  .edit-btn:hover {
    color: white;
    background-color: #6bdfeeff;
  }

  /* Edit-Modus */
  .edit-mode {
    border: 2px solid #6bdfeeff !important;
    background-color: #f0fff0 !important;
  }

  .action-btn-container {
    display: flex;
    justify-content: space-between;
    margin-top: 10px;
    gap: 4%;
  }

  /* Autosave Notification */
  .autosave-notification {
    position: fixed;
    bottom: 20px;
    right: 20px;
    padding: 10px 20px;
    background-color: #4CAF50;
    color: white;
    border-radius: 5px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    display: none;
    z-index: 1000;
  }

   /* Autosave Status Display */
  .autosave-status {
    position: fixed;
    bottom: 70px;
    right: 20px;
    padding: 8px 15px;
    background-color: #28a745;
    color: white;
    border-radius: 3px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    font-size: 12px;
    display: none;
    z-index: 1001;
  }

  .autosave-error {
    background-color: #dc3545 !important;
  }

.recovery-panel {
  background-color: #f7dc84ff;
  border: 1px solid #fcbf3bff;
  border-radius: 5px;
  padding: 10px;
  margin: 10px 0;
} 

  /* Sidebar buttons */
  .sidebar-buttons {
    display: flex;
    flex-direction: column;
    gap: 10px;
    margin-top: 15px;
  }
  .sidebar-buttons .btn { width: 100%; margin-bottom: 5px; }
  .sidebar-buttons .shiny-input-container { width: 100%; }

  /* Shortcut notification */
  .shortcut-notification {
    position: fixed;
    bottom: 70px;
    right: 20px;
    padding: 8px 15px;
    background-color: #333;
    color: white;
    border-radius: 3px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.2);
    font-size: 12px;
    display: none;
    z-index: 1001;
  }

  /* Shortcuts help panel */
  .shortcuts-help {
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 5px;
    padding: 15px;
    margin-top: 10px;
    font-size: 12px;
  }

  /* Video Player Mausrad-Interaktion */
  #videoPlayer { cursor: grab; }
  #videoPlayer:active { cursor: grabbing; }

  /* Keyboard shortcuts icon */
.shortcuts-icon {
  position: absolute;
  right: 10px;
  bottom: 10px;
  display: inline-block;
  cursor: pointer;
  color: #6c757d;
  font-size: 18px;
  z-index: 100;
}

.shortcuts-icon:hover {
  color: #68a3fcff;
}

/* Video controls container positioning */
.video-controls {
  position: relative;
}

/* Tooltip für Shortcuts */
.shortcuts-tooltip {
  visibility: hidden;
  width: 350px;
  background-color: #f8f9fa;
  color: #333;
  text-align: left;
  border-radius: 6px;
  padding: 15px;
  border: 1px solid #dee2e6;
  position: absolute;
  z-index: 1000;
  bottom: 125%;
  right: 0;
  opacity: 0;
  transition: opacity 0.3s;
  font-size: 12px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

.shortcuts-tooltip::after {
  content: "";
  position: absolute;
  top: 100%;
  right: 20px;
  border-width: 5px;
  border-style: solid;
  border-color: #f8f9fa transparent transparent transparent;
}

.shortcuts-icon:hover .shortcuts-tooltip {
  visibility: visible;
  opacity: 1;
}

/* Button-Container für erste Spalte */
.action-buttons-column {
  min-width: 120px;
  white-space: nowrap;
}

.action-buttons-column .edit-btn,
.action-buttons-column .delete-btn {
  margin-right: 3px;
  padding: 3px 8px;
  font-size: 11px;
}

')),

tags$script(HTML("
  // Globales Objekt zum Speichern der Video-URLs
  var videoStorage = {};
  
  $(document).ready(function() {
    // Speed control für Video
    Shiny.addCustomMessageHandler('updateSpeed', function(message) {
      var video = document.getElementById('videoPlayer');
      if (video) {
        video.playbackRate = message.speed;
      }
    });
    
    // Zu einer bestimmten Zeit springen
    Shiny.addCustomMessageHandler('seekToTime', function(message) {
      var video = document.getElementById('videoPlayer');
      if (video) {
        video.currentTime = message.time;
        console.log('Jumped to time:', message.time); // Debug output
      }
    });
    
    // Function to show shortcut notifications
    function showShortcutNotification(text) {
      if ($('#shortcutNotification').length === 0) {
        $('body').append('<div id=\"shortcutNotification\" class=\"shortcut-notification\"></div>');
      }
      $('#shortcutNotification').text(text).fadeIn().delay(1000).fadeOut();
    }
    
    // Keyboard Shortcuts
    $(document).keydown(function(e) {
      var video = document.getElementById('videoPlayer');
      if (!video) return;
      
      // Prevent shortcuts when typing in input fields
      if ($(e.target).is('input, textarea, select')) return;
      
      switch(e.which) {
        case 32: // Spacebar - Play/Pause
          e.preventDefault();
          if (video.paused) {
            video.play();
            showShortcutNotification('Play');
          } else {
            video.pause();
            showShortcutNotification('Pause');
          }
          break;
          
        case 37: // Left Arrow - Skip backward 5s
          e.preventDefault();
          video.currentTime = Math.max(0, video.currentTime - 5);
          showShortcutNotification('-5s');
          break;
          
        case 39: // Right Arrow - Skip forward 5s
          e.preventDefault();
          video.currentTime = Math.min(video.duration, video.currentTime + 5);
          showShortcutNotification('+5s');
          break;
          
        case 40: // Arrow Down ↓
          e.preventDefault();
          video.currentTime = Math.max(0, video.currentTime - 0.033);
          showShortcutNotification('-1 Frame');
          break;
          
        case 38: // Arrow Up ↑ - Skip forward 1 frame (~0.033s at 30fps)
          e.preventDefault();
          video.currentTime = Math.min(video.duration, video.currentTime + 0.033);
          showShortcutNotification('+1 Frame');
          break;

          
        case 65: // A - Mark start time
          if (e.ctrlKey || e.metaKey) return; // Don't interfere with Ctrl+A
          e.preventDefault();
          $('#markStartTime').click();
          showShortcutNotification('Start markiert');
          break;
          
        case 66: // B - Mark end time
          e.preventDefault();
          $('#markEndTime').click();
          showShortcutNotification('Ende markiert');
          break;
          
        case 84: // T - Add tag
          e.preventDefault();
          $('#add_tag').click();
          showShortcutNotification('Event getaggt');
          break;
      }
    });
    
    // Mouse Wheel Controls - Video scrubbing
    $(document).on('wheel', '#videoPlayer', function(e) {
      e.preventDefault();
      var video = document.getElementById('videoPlayer');
      if (!video) return;
      
      var delta = e.originalEvent.deltaY;
      
      if (e.originalEvent.ctrlKey || e.originalEvent.metaKey) {
        // Mit Ctrl/Cmd gedrückt: Geschwindigkeit ändern
        var currentSpeed = video.playbackRate;
        var newSpeed;
        
        if (delta < 0) { // Scroll up - increase speed
          newSpeed = Math.min(2, currentSpeed + 0.25);
        } else { // Scroll down - decrease speed  
          newSpeed = Math.max(0.25, currentSpeed - 0.25);
        }
        
        video.playbackRate = newSpeed;
        $('#videoSpeed').val(newSpeed).trigger('change');
        showShortcutNotification(newSpeed + 'x Speed');
        
      } else if (e.originalEvent.shiftKey) {
        // Mit Shift gedrückt: Frame-by-Frame Navigation
        if (delta < 0) { // Scroll up - next frame
          video.currentTime = Math.min(video.duration, video.currentTime + 0.033);
          showShortcutNotification('+1 Frame');
        } else { // Scroll down - previous frame
          video.currentTime = Math.max(0, video.currentTime - 0.033);
          showShortcutNotification('-1 Frame');
        }
        
      } else {
        // Normal: Video scrubbing (±1 Sekunde)
        if (delta < 0) { // Scroll up - forward
          video.currentTime = Math.min(video.duration, video.currentTime + 1);
          showShortcutNotification('+1s');
        } else { // Scroll down - backward
          video.currentTime = Math.max(0, video.currentTime - 1);
          showShortcutNotification('-1s');
        }
      }
    });

    // Video File Upload Handler
    $('#videoFile').on('change', function(e) {
      var file = e.target.files[0];
      if (file) {
        var objectURL = URL.createObjectURL(file);
        $('#videoPlayer').attr('src', objectURL);
        
        // Speichere Video URL mit Dateinamen als Schlüssel
        videoStorage[file.name] = objectURL;
        
        // Sende Liste der verfügbaren Videos an Shiny
        var videoNamesList = Object.keys(videoStorage);
        Shiny.setInputValue('videoNames', videoNamesList);
        
        // Setze den aktuellen Videowert (für die Dropdown-Liste)
        Shiny.setInputValue('currentVideoName', file.name);
      }
    });
    
    // Wenn ein Video aus der Dropdown-Liste ausgewählt wird
    Shiny.addCustomMessageHandler('loadSelectedVideo', function(message) {
      var videoName = message.name;
      if (videoStorage[videoName]) {
        $('#videoPlayer').attr('src', videoStorage[videoName]);
      }
    });
    
    // Direkt beim Tagging die aktuelle Videozeit senden
    $('#add_tag').on('click', function() {
      var video = document.getElementById('videoPlayer');
      if (video) {
        Shiny.setInputValue('tagCurrentTime', video.currentTime);
      }
    });

    // Handler für Hajime Button - aktuelle Zeit direkt erfassen
    $('#add_hajime').on('click', function() {
      var video = document.getElementById('videoPlayer');
      if (video) {
        Shiny.setInputValue('hajime_current_time', video.currentTime);
      }
    });

    // Handler für Mate Button - aktuelle Zeit direkt erfassen  
    $('#add_mate').on('click', function() {
      var video = document.getElementById('videoPlayer');
      if (video) {
        Shiny.setInputValue('mate_current_time', video.currentTime);
      }
    });

    // Delegierte Event-Handler für Delete- und Edit-Buttons
    $(document).on('click', '.delete-btn', function(e) {
      e.stopPropagation(); // Verhindert, dass das Event zum Table-Row-Click bubbled
      var timeValue = $(this).data('time');
      Shiny.setInputValue('delete_row', timeValue);
    });

    $(document).on('click', '.edit-btn', function(e) {
      e.stopPropagation(); // Verhindert, dass das Event zum Table-Row-Click bubbled
      var timeValue = $(this).data('time');
      Shiny.setInputValue('edit_row', timeValue);
    });

    // Aktuelle Videozeit kontinuierlich anzeigen
    setInterval(function() {
      var video = document.getElementById('videoPlayer');
      if (video) {
        Shiny.setInputValue('currentVideoTime', video.currentTime);
      }
    }, 500); // alle 500ms aktualisieren
    
    // Autosave-Benachrichtigung anzeigen
    Shiny.addCustomMessageHandler('showAutosaveNotification', function(message) {
      // Erstelle Benachrichtigung, falls noch nicht vorhanden
      if ($('#autosaveNotification').length === 0) {
        $('body').append('<div id=\"autosaveNotification\" class=\"autosave-notification\"><i class=\"fas fa-save\"></i> ' + message.text + '</div>');
      }
      
      // Zeige Benachrichtigung
      $('#autosaveNotification').text(message.text).fadeIn().delay(2000).fadeOut();
    });
  });
"))

 ), 
  ## Sidebar ####
  sidebar = sidebar(
    width = "20%",
    position = "right",
    open = FALSE,
    title = useShinyjs(),
    # Füge neue Datenmanagement-Sektion hinzu,
    div(
        style = "padding: 15px 0;",
        h4("Datenmanagement"),
        div(
          class = "sidebar-buttons",
          downloadButton("download_tags", "Events exportieren"),
          downloadButton("download_special_format", "T-Daten exportieren"),
          downloadButton("save_project", "Projekt speichern"),
          fileInput("load_project", "Projekt laden", accept = ".rds"),
          uiOutput("restore_autosave"),
          hr(),
          actionButton("clear_all", "Alles löschen", class = "btn-danger", style = "width: 100%;")
        )
      )
  ),
  collapsible = TRUE,
  
  ## Analyse Seite ####,
  nav_panel(
      "Analyse",
      # Erste Zeile mit Video und Event Tagging
      fluidRow(
        column(6,
  card(height = "100%",
    card_header("Video"),
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        open = FALSE,
        class = "player-sidebar",
          h5("Geladene Videos"),
          uiOutput("videoSelectionDropdown"),
          hr(),
          h5("Videoschnitt"),
          actionButton("markStartTime", "Markiere Anfang (A)", class = "btn-sm btn-primary", style = "width: 100%; margin-bottom: 10px"),
          verbatimTextOutput("startTimeDisplay"),
          actionButton("markEndTime", "Markiere Ende (B)", class = "btn-sm btn-primary", style = "width: 100%; margin-bottom: 10px"),
          verbatimTextOutput("endTimeDisplay"),
          hr(),
          actionButton("extractClip", "Sequenz extrahieren", class = "btn-success", style = "width: 100%")
        ),
      fluidRow(
        column(12,
          tags$input(
            id = "videoFile",
            type = "file",
            accept = "video/mp4,video/webm,video/ogg",
            class = "form-control mb-3"
          )
        )
      ),
       tags$video(
        id = "videoPlayer",
        width = "100%",
        controls = TRUE,
        src = ""
      ),
      textOutput("currentTimeDisplay"),

      # Keyboard Shortcuts Icon mit Tooltip
      div(class = "shortcuts-icon",
        tags$i(class = "fas fa-keyboard"),
        div(class = "shortcuts-tooltip",
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
            div(
              tags$strong("Video-Steuerung:"), tags$br(),
              "Leertaste: Play/Pause", tags$br(),
              "← →: ±5 Sekunden", tags$br(),
              "↑ ↓: ±1 Frame", tags$br(),
              tags$strong("Mausrad:"), tags$br(),
              "Normal: ±1 Sekunde", tags$br(),
              "Shift+Rad: ±1 Frame", tags$br(),
              "Ctrl+Rad: Videogeschwindigkeit"
            ),
            div(
              tags$strong("Tagging:"), tags$br(), 
              "A: Start markieren", tags$br(),
              "B: Ende markieren", tags$br(),
              "T: Event taggen"
            )
          )
        )
      )
    )
  )
),
        column(6,  
               card(height = "100%",
                 card_header("Event Tagging"),
                 # In der UI, in der card("Event Tagging"), in der fluidRow mit den RadioButtons:
                    fluidRow(
                      column(3,  # Spaltenbreite angepasst 3
                            radioButtons("rolle", "Judoka",
                                          choices = c("Blau" = "B", "Weiss" = "W"),
                                          selected = "B")
                      ),
                      column(3,  # Spaltenbreite angepasst 3
                            radioButtons("zeitpunkt", "Zeitpunkt",
                                        choices = c("Beginn" = "b", "Ende" = "e"),
                                        selected = "b")
                      ),
                      
                      column(3,
                            radioButtons("wertungsstand", "Wertungsstand",
                                        choices = c("Führung" = "WIN", "Gleichstand" = "PARI", "Rückstand" = "LOSE"),
                                        selected = "PARI")
                      ),
                      column(3,  # Spaltenbreite angepasst 3
                            selectInput("main_category", "Phase:",
                                        choices = NULL)
                      )
                    ),
                 uiOutput("criteria_ui"),
                 actionButton("add_tag", "Event taggen", class = "btn-primary"),
                 div(class = "action-btn-container",
                  actionButton("add_hajime", "Hajime", 
                              class = "btn-success", 
                              style = "background-color: #6bdfeeff; border-color: #6bdfeeff; width: 48%;"),
                  actionButton("add_mate", "Mate", 
                              class = "btn-danger", 
                              style = "background-color: #e26c78ff; border-color: #e26c78ff; width: 48%;")
                                )
          )
        )
      ),
      # Zweite Zeile mit der Tabelle über volle Breite
      fluidRow(
        column(12,
               card(
                 card_header("Getaggte Events"),
                 div(
                   class = "event-list",
                   DTOutput("event_list")
                 )
               )
            )
      )
    ),
  ## Auswertung Seite ####,
nav_panel(
  "Auswertung",
  card(
    navset_tab(
      nav_panel("Überblick", 
                p("Allgemeiner Überblick über die gesammelten Daten."),
                plotOutput("overview_plot")),
      nav_panel("Zeitanalyse", 
                p("Analyse der Events über die Zeit."),
                plotOutput("time_analysis_plot")),
      nav_panel("Statistiken", 
                p("Statistische Auswertung der getaggten Events."),
                verbatimTextOutput("stats_output")),
      nav_panel("Trendanalyse", 
                p("Trendanalyse der erfassten Techniken und Bewegungen."),
                plotOutput("trend_plot")),
      nav_menu(
        "Erweiterte Analysen",
        nav_panel("Vergleichsanalyse", 
                  p("Vergleich verschiedener Wettkämpfe oder Judoka."),
                  plotOutput("comparison_plot")),
        "----",
        "Externe Ressourcen:",
        nav_item(
          a("Judo-Techniken", href = "https://www.ijf.org/ijf/education/technical-analysis", target = "_blank")
        ),
        nav_item(
          a("Datenanalyse-Dokumentation", href = "https://shiny.posit.co/r/gallery/", target = "_blank")
        )
      )
    ),
    id = "analysis_tabs"
  )
),
  # Handbuch Tagging-Regeln
  nav_panel(
    "Handbuch",
    tags$iframe(
      src = "https://staticcloud.sport-iat.de/judo/Handbuch/index.html",  # Pfad relativ zum www-Ordner
      width = "100%",
      height = "900px",
      frameborder = "0",
      style = "border:none;"
    )
  )
)


# Server für Shiny App ####
server <- function(input, output, session) {
  # Reaktive Werte für die Anwendung
rv <- reactiveValues(
  events = data.frame(
    Zeit = numeric(0),
    FPS = numeric(0),
    Rolle = character(0),
    Wertungsstand = character(0),
    Phase = character(0),
    Trennkommando = character(0),
    stringsAsFactors = FALSE
  ),
  current_video = NULL,
  editing = FALSE,      # Flag für Bearbeitungsmodus
  edit_index = NULL     # Index der zu bearbeitenden Zeile
)
  
  # Zeit in FPS #
  formatTime <- function(seconds) {
    total_frames <- round(seconds * 30)
    return(total_frames)
  }
  
  # Aktualisiere die Anzeige der aktuellen Videozeit
  output$currentTimeDisplay <- renderText({
    req(input$currentVideoTime)
    paste("Zeit:", formatTime(input$currentVideoTime))
  })

  # Reaktive Werte für Videoschnitt
  rv$clipStartTime <- NULL
  rv$clipEndTime <- NULL

  # Markiere Anfang der Videosequenz
  observeEvent(input$markStartTime, {
    req(input$currentVideoTime)
    rv$clipStartTime <- input$currentVideoTime
  })

  # Markiere Ende der Videosequenz
  observeEvent(input$markEndTime, {
    req(input$currentVideoTime)
    rv$clipEndTime <- input$currentVideoTime
  })

  # Zeige Start- und Endzeit an
  output$startTimeDisplay <- renderText({
    if (is.null(rv$clipStartTime)) return("Noch nicht markiert")
    paste0("A: ", round(rv$clipStartTime, 1), " s (", formatTime(rv$clipStartTime), " Frames)")
  })

  output$endTimeDisplay <- renderText({
    if (is.null(rv$clipEndTime)) return("Noch nicht markiert")
    paste0("B: ", round(rv$clipEndTime, 1), " s (", formatTime(rv$clipEndTime), " Frames)")
  })

  # Sequenz extrahieren
  observeEvent(input$extractClip, {
    req(rv$clipStartTime, rv$clipEndTime, input$selectedVideo)
    
    if (rv$clipStartTime >= rv$clipEndTime) {
      showNotification("Endzeit muss nach Startzeit liegen", type = "error")
      return()
    }
    
    # JavaScript-Funktion zum Extrahieren der Videoclips
    session$sendCustomMessage("extractVideoClip", list(
      startTime = rv$clipStartTime,
      endTime = rv$clipEndTime,
      videoId = "videoPlayer",
      outputName = paste0("clip_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".mp4")
    ))
    
    showNotification("Videosequenz wird extrahiert...", type = "message", duration = 5)
  })

  
  # Dropdown für die Videoauswahl aktualisieren
  output$videoSelectionDropdown <- renderUI({
    req(input$videoNames)
    selectInput("selectedVideo", "Geladene Videos:", 
                choices = input$videoNames,
                selected = input$currentVideoName)
  })
  
  # Video wechseln, wenn ein anderes aus der Dropdown-Liste ausgewählt wird
  observeEvent(input$selectedVideo, {
    req(input$selectedVideo)
    if (input$selectedVideo != "") {
      session$sendCustomMessage("loadSelectedVideo", list(name = input$selectedVideo))
      rv$current_video <- input$selectedVideo
    }
  })
  
# Update main category choices based on selected role
        observe({
          role <- input$rolle
          
          if (!is.null(role) && role %in% names(categories)) {
            main_cats <- names(categories[[role]])
            updateSelectInput(session, "main_category", choices = main_cats)
          }
        })
  
# Dynamisches UI für die Kriterien basierend auf der ausgewählten Hauptkategorie erstellen
output$criteria_ui <- renderUI({
  req(input$rolle, input$main_category)
  
  role <- input$rolle
  main_cat <- input$main_category
  
  if (is.null(categories[[role]][[main_cat]])) {
    return(NULL)
  }
  
  criteria_list <- categories[[role]][[main_cat]]
  
  # Erstelle UI-Elemente für jedes Kriterium
  criteria_uis <- lapply(names(criteria_list), function(criterion_name) {
    criterion_info <- criteria_list[[criterion_name]]
    input_id <- paste0("criterion_", gsub(" ", "_", criterion_name))
    
    # Je nach definiertem Typ das passende UI-Element erstellen
    if (criterion_info$type == "select") {
      selectInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    } else if (criterion_info$type == "selectize") {
      selectizeInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1],
        options = list(
          placeholder = paste("Wähle", criterion_name),
          create = TRUE,
          createOnBlur = TRUE
        )
      )
    } else if (criterion_info$type == "radio") {
      radioButtons(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1],
        inline = TRUE
      )
    } else if (criterion_info$type == "checkbox") {
      checkboxGroupInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    } else {
      # Fallback auf select
      selectInput(
        inputId = input_id,
        label = criterion_name,
        choices = criterion_info$choices,
        selected = criterion_info$choices[1]
      )
    }
  })
  
  # Gibt alle UI-Elemente zurück
  do.call(tagList, criteria_uis)
})
  
# Event hinzufügen oder aktualisieren, wenn der Button geklickt wird
observeEvent(input$add_tag, {
  req(input$rolle, input$main_category)
  
  # Sammle alle Kriterien-Werte
  role <- input$rolle  # Direkt verwenden
  main_cat <- input$main_category
  
  # Zeit vom Video oder aus bestehendem Event
  time_point <- if (rv$editing && !is.null(rv$edit_index)) {
    rv$events$Zeit[rv$edit_index]
  } else {
    req(input$tagCurrentTime)
    input$tagCurrentTime
  }
  
  time_formatted <- formatTime(time_point)
  
  # Hole die Kriterien für diese Kategorie
  criteria_names <- names(categories[[role]][[main_cat]])
  
  # Erstelle eine Liste der gewählten Werte
  selected_values <- list()
  for (criterion in criteria_names) {
    input_id <- paste0("criterion_", gsub(" ", "_", criterion))
    if (!is.null(input[[input_id]])) {
      selected_values[[criterion]] <- input[[input_id]]
    }
  }
  
  # Erstelle einen neuen Eintrag für die Datentabelle
  new_row <- data.frame(
    Zeit = time_point,
    FPS = time_formatted,
    Rolle = input$rolle,
    Zeitpunkt = input$zeitpunkt,
    Wertungsstand = input$wertungsstand,
    Phase = main_cat,
    stringsAsFactors = FALSE
  )
  
  # Füge ausgewählte Kriterien hinzu
  for (criterion in names(selected_values)) {
    # Wenn es sich um einen Vektor handelt (z.B. bei Checkboxen), verbinde die Werte
    if (length(selected_values[[criterion]]) > 1) {
      new_row[[criterion]] <- paste(selected_values[[criterion]], collapse = ", ")
    } else {
      new_row[[criterion]] <- selected_values[[criterion]]
    }
  }
  
  # Im Bearbeitungsmodus: ersetze die existierende Zeile
  if (rv$editing && !is.null(rv$edit_index)) {
    # Stelle sicher, dass alle Spalten in beiden Dataframes vorhanden sind
    for (col in names(new_row)) {
      if (!col %in% names(rv$events)) {
        rv$events[[col]] <- NA
      }
    }
    for (col in names(rv$events)) {
      if (!col %in% names(new_row)) {
        new_row[[col]] <- NA
      }
    }
    
    # Ersetze die Zeile
    rv$events[rv$edit_index, names(new_row)] <- new_row
    
    # Bearbeitungsmodus zurücksetzen
    rv$editing <- FALSE
    rv$edit_index <- NULL
    
    # Button-Erscheinungsbild zurücksetzen
    shinyjs::removeClass(selector = "#add_tag", class = "btn-success")
    shinyjs::html("add_tag", "Event taggen")
    
    showNotification("Event wurde aktualisiert", type = "message")
  } else {
    # Im normalen Modus: füge eine neue Zeile hinzu
    if (nrow(rv$events) == 0) {
      rv$events <- new_row
    } else {
      # Stelle sicher, dass alle Spalten in beiden Dataframes vorhanden sind
      for (col in names(new_row)) {
        if (!col %in% names(rv$events)) {
          rv$events[[col]] <- NA
        }
      }
      for (col in names(rv$events)) {
        if (!col %in% names(new_row)) {
          new_row[[col]] <- NA
        }
      }
      
      rv$events <- rbind(rv$events, new_row)
    }
  }
  
  # Sortiere Events nach Zeit
  rv$events <- rv$events[order(rv$events$Zeit, decreasing= TRUE ), ]
})

# Hajime Event hinzufügen
observeEvent(input$add_hajime, {
  req(input$hajime_current_time)  # Geändert von input$tagCurrentTime
  
  time_point <- input$hajime_current_time
  time_formatted <- formatTime(time_point)
  
  # Erstelle einen neuen Hajime-Eintrag
  new_row <- data.frame(
    Zeit = time_point,
    FPS = time_formatted,
    Rolle = NA,
    Zeitpunkt = NA,
    Phase = NA,
    Trennkommando = "Hajime",
    stringsAsFactors = FALSE
  )
  
  # Füge zur Tabelle hinzu
  if (nrow(rv$events) == 0) {
    rv$events <- new_row
  } else {
    # Stelle sicher, dass alle Spalten vorhanden sind
    for (col in names(new_row)) {
      if (!col %in% names(rv$events)) {
        rv$events[[col]] <- NA
      }
    }
    for (col in names(rv$events)) {
      if (!col %in% names(new_row)) {
        new_row[[col]] <- NA
      }
    }
    rv$events <- rbind(rv$events, new_row)
  }
  
  # Sortiere nach Zeit
  rv$events <- rv$events[order(rv$events$Zeit), ]
  
  showNotification("Hajime-Kommando hinzugefügt", type = "message")
})

# Mate Event hinzufügen
observeEvent(input$add_mate, {
  req(input$mate_current_time)  # Geändert von input$tagCurrentTime
  
  time_point <- input$mate_current_time
  time_formatted <- formatTime(time_point)
  
  # Erstelle einen neuen Mate-Eintrag
  new_row <- data.frame(
    Zeit = time_point,
    FPS = time_formatted,
    Rolle = NA,
    Zeitpunkt = NA,
    Phase = NA,
    Trennkommando = "Mate",
    stringsAsFactors = FALSE
  )
  
  # Füge zur Tabelle hinzu
  if (nrow(rv$events) == 0) {
    rv$events <- new_row
  } else {
    # Stelle sicher, dass alle Spalten vorhanden sind
    for (col in names(new_row)) {
      if (!col %in% names(rv$events)) {
        rv$events[[col]] <- NA
      }
    }
    for (col in names(rv$events)) {
      if (!col %in% names(new_row)) {
        new_row[[col]] <- NA
      }
    }
    rv$events <- rbind(rv$events, new_row)
  }
  
  # Sortiere nach Zeit
  rv$events <- rv$events[order(rv$events$Zeit), ]
  
  showNotification("Mate-Kommando hinzugefügt", type = "message")
})
  
# Anzeige der getaggten Events in einer Tabelle
output$event_list <- renderDT({
  req(rv$events)
  
  if (nrow(rv$events) == 0) {
    return(NULL)
  }
  
  # Formatiere Tabelle für Anzeige
  display_df <- rv$events

  # Runde die Zeit-Spalte auf eine Dezimalstelle
  display_df$Zeit <- round(display_df$Zeit * 10) / 10

  # Phase abkürzen
  for (phase_full in names(phase_abbreviations)) {
    display_df$Phase[display_df$Phase == phase_full] <- phase_abbreviations[phase_full]
  }
  
  # Füge Aktions-Spalte als erste Spalte hinzu
  action_column <- sapply(display_df$Zeit, function(time) {
    sprintf('<div class="action-buttons-column"><button class="btn btn-sm edit-btn" data-time="%s"><i class="fa fa-edit"></i></button><button class="btn btn-sm delete-btn" data-time="%s"><i class="fa fa-trash"></i></button></div>', time, time)
  })

  # Erstelle neuen Dataframe mit Aktions-Spalte an erster Position
  display_df <- data.frame(
    Editieren = action_column,
    display_df,
    stringsAsFactors = FALSE
  )
  
  # Datatable mit verbessertem JavaScript
  datatable(
    display_df,
    options = list(
      pageLength = 15,
      lengthMenu = c(15, 30, 50, 100),
      dom = 'lftip',
      columnDefs = list(
        list(orderable = FALSE, targets = 0)  # Erste Spalte nicht sortierbar
      ),
    rowCallback = JS("
      function(row, data) {
        $(row).addClass('clickable-row');
        
        // Remove old event handlers to prevent duplicates
        $(row).off('click.rowClick');
        $('.edit-btn, .delete-btn', row).off('click');
        
        // Row-Click Handler for jumping to video time
        $(row).on('click.rowClick', function(e) {
          // Only trigger if we didn't click on a button or icon
          if (!$(e.target).hasClass('delete-btn') && !$(e.target).closest('.delete-btn').length &&
              !$(e.target).hasClass('edit-btn') && !$(e.target).closest('.edit-btn').length &&
              !$(e.target).hasClass('fa') && !$(e.target).closest('button').length) {
            
            // Get the time value from the edit button's data-time attribute (original precise value)
            var timeValue = $('.edit-btn', row).data('time');
            if (timeValue !== undefined && timeValue !== null) {
              Shiny.setInputValue('selected_time', parseFloat(timeValue), {priority: 'event'});
            }
          }
        });
        
        // Edit Button Handler
        $('.edit-btn', row).on('click', function(e) {
          e.preventDefault();
          e.stopPropagation();
          var timeValue = $(this).data('time');
          Shiny.setInputValue('edit_row', timeValue, {priority: 'event'});
        });
        
        // Delete Button Handler  
        $('.delete-btn', row).on('click', function(e) {
          e.preventDefault();
          e.stopPropagation();
          var timeValue = $(this).data('time');
          Shiny.setInputValue('delete_row', timeValue, {priority: 'event'});
        });
        
        // Format time column as clickable (Zeit column is at index 2)
        $('td:eq(2)', row).addClass('time-display');
      }
    ")
    ),
    selection = 'none',
    rownames = FALSE,
    escape = FALSE
  )
})
  
  # Zum ausgewählten Zeitpunkt im Video springen
# Jump to selected time in video
  observeEvent(input$selected_time, {
    req(input$selected_time)
    print(paste("Jumping to time:", input$selected_time))  # Debug output
    session$sendCustomMessage("seekToTime", list(time = as.numeric(input$selected_time)))
  })
  
# Einzelne Zeile löschen
observeEvent(input$delete_row, {
  req(input$delete_row)
  time_to_delete <- as.numeric(input$delete_row)
  
  # Finde den Index der zu löschenden Zeile - präziserer Vergleich
  # Benutze which.min() um den genauesten Treffer zu finden
  differences <- abs(rv$events$Zeit - time_to_delete)
  if(length(differences) > 0 && min(differences) < 0.1) { # Toleranz von 0.1 Sekunden
    row_index <- which.min(differences)
    
    # Lösche die Zeile
    rv$events <- rv$events[-row_index, , drop = FALSE]
    
    # Melde Erfolg
    showNotification("Event wurde gelöscht", type = "message")
  } else {
    # Melde Fehler wenn keine passende Zeit gefunden wurde
    showNotification("Konnte kein passendes Event finden", type = "error")
  }
})

# Bearbeitungsmodus aktivieren
observeEvent(input$edit_row, {
  req(input$edit_row)
  time_to_edit <- as.numeric(input$edit_row)
  
  # Finde den Index der zu bearbeitenden Zeile
  differences <- abs(rv$events$Zeit - time_to_edit)
  if(length(differences) > 0 && min(differences) < 0.1) { # Toleranz von 0.1 Sekunden
    row_index <- which.min(differences)
    
    # Setze Bearbeitungsmodus
    rv$editing <- TRUE
    rv$edit_index <- row_index
    
    # Event Daten laden
    event_data <- rv$events[row_index, ]
    
    # Debug-Ausgabe
    print(paste("Bearbeite Event an Zeit:", event_data$Zeit))
    print(event_data)
    
    # Rolle und Hauptkategorie aktualisieren
    updateRadioButtons(session, "rolle", selected = event_data$Rolle)
    updateRadioButtons(session, "zeitpunkt", selected = event_data$Zeitpunkt)
    updateRadioButtons(session, "wertungsstand", selected = event_data$Wertungsstand)
    updateSelectInput(session, "main_category", selected = event_data$Phase)
    
    # Verzögerung erhöhen und überprüfen, ob die Hauptkategorie korrekt gesetzt wurde
    shinyjs::delay(500, {
      if (input$main_category != event_data$Phase) {
        updateSelectInput(session, "main_category", selected = event_data$Phase)
      }
      
      # Nochmals verzögert die Kriterien aktualisieren
      shinyjs::delay(300, {
        # Alle Kriterien für diese Kategorie durchlaufen
        criteria_names <- names(categories[[event_data$Rolle]][[event_data$Phase]])
        for (criterion in criteria_names) {
          input_id <- paste0("criterion_", gsub(" ", "_", criterion))
          
          # Wenn der Wert in den Event-Daten existiert
          if (criterion %in% names(event_data) && !is.na(event_data[[criterion]])) {
            # Feld-Typ bestimmen
            criterion_type <- categories[[event_data$Rolle]][[event_data$Phase]][[criterion]]$type
            
            print(paste("Aktualisiere Feld:", input_id, "mit Wert:", event_data[[criterion]]))
            
            if (criterion_type == "select" || criterion_type == "selectize") {
              updateSelectInput(session, input_id, selected = event_data[[criterion]])
            } else if (criterion_type == "radio") {
              updateRadioButtons(session, input_id, selected = event_data[[criterion]])
            } else if (criterion_type == "checkbox") {
              # Bei Checkbox die Werte teilen
              selected_values <- unlist(strsplit(event_data[[criterion]], ", "))
              updateCheckboxGroupInput(session, input_id, selected = selected_values)
            }
          }
        }
        
        # Button-Erscheinungsbild ändern
        shinyjs::addClass(selector = "#add_tag", class = "btn-success")
        shinyjs::html("add_tag", "Event aktualisieren")
      })
    })
    
    # Zum Zeitpunkt im Video springen
    session$sendCustomMessage("seekToTime", list(time = event_data$Zeit))
    
    # Melde Erfolg
    showNotification("Event wird bearbeitet. Aktualisieren Sie die Werte und klicken Sie auf 'Event aktualisieren'.", 
                     type = "message", duration = 5)
  }
})  

# Platzhalter für Auswertungs-Outputs
output$overview_plot <- renderPlot({
  # Placeholder für den Überblicksplot
  plot(rv$events$Zeit, runif(nrow(rv$events)), 
       main = "Überblick der Events", 
       xlab = "Zeit", ylab = "Wert",
       col = ifelse(rv$events$Rolle == "Blau", "blue", "red"),
       pch = 19)
  if(nrow(rv$events) > 0) {
    legend("topright", legend = c("Blau", "Weiss"), 
           col = c("blue", "red"), pch = 19)
  }
})

output$time_analysis_plot <- renderPlot({
  # Placeholder für Zeitanalyse
  if(nrow(rv$events) > 0) {
    hist(rv$events$Zeit, 
         main = "Verteilung der Events über die Zeit",
         xlab = "Zeit (s)", col = "skyblue", border = "white")
  } else {
    plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Keine Daten verfügbar")
  }
})

output$stats_output <- renderPrint({
  # Placeholder für statistische Zusammenfassung
  if(nrow(rv$events) > 0) {
    cat("Anzahl der Events:", nrow(rv$events), "\n\n")
    
    # Anzahl pro Rolle
    role_counts <- table(rv$events$Rolle)
    cat("Events pro Judoka:\n")
    print(role_counts)
    cat("\n")
    
    # Anzahl pro Phase
    phase_counts <- table(rv$events$Phase)
    cat("Events pro Phase:\n")
    print(phase_counts)
  } else {
    cat("Keine Daten verfügbar für die statistische Analyse.")
  }
})

output$trend_plot <- renderPlot({
  # Placeholder für Trendanalyse
  if(nrow(rv$events) > 0) {
    # Einfacher Plot der Events nach Phase
    phases <- unique(rv$events$Phase)
    counts <- sapply(phases, function(p) sum(rv$events$Phase == p))
    barplot(counts, names.arg = phases, 
            main = "Events pro Phase", 
            col = rainbow(length(phases)),
            las = 2, cex.names = 0.8)
  } else {
    plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Keine Daten verfügbar")
  }
})

output$comparison_plot <- renderPlot({
  # Placeholder für Vergleichsanalyse
  if(nrow(rv$events) > 0 && length(unique(rv$events$Rolle)) > 1) {
    # Vergleich der Events nach Rolle und Phase
    roles <- unique(rv$events$Rolle)
    phases <- unique(rv$events$Phase)
    
    # Erstelle Datenmatrix für den Vergleich
    comparison_data <- matrix(0, nrow = length(roles), ncol = length(phases))
    rownames(comparison_data) <- roles
    colnames(comparison_data) <- phases
    
    for(i in 1:length(roles)) {
      for(j in 1:length(phases)) {
        comparison_data[i, j] <- sum(rv$events$Rolle == roles[i] & rv$events$Phase == phases[j])
      }
    }
    
    barplot(comparison_data, beside = TRUE, 
            main = "Vergleich der Events nach Judoka und Phase",
            col = c("blue", "red"),
            legend.text = roles,
            args.legend = list(x = "topright"))
  } else {
    plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
         main = "Nicht genügend Daten für einen Vergleich")
  }
})


  
  # Alle Events löschen
  observeEvent(input$clear_all, {
    showModal(modalDialog(
      title = "Events löschen",
      "Möchtest du wirklich alle getaggten Events löschen?",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirm_clear", "Löschen", class = "btn-danger")
      )
    ))
  })
  
  # Bestätigung zum Löschen aller Events
  observeEvent(input$confirm_clear, {
    rv$events <- data.frame(
      Zeit = numeric(0),
      FPS = numeric(0),  # Korrigiert von "ZeitFormatiert" zu "FPS"
      Rolle = character(0),
      Zeitpunkt = character(0),
      Wertungsstand = character(0),
      Phase = character(0),
      Trennkommando = character(0),
      stringsAsFactors = FALSE
    )
    removeModal()
  })
  
  # Download der Events als CSV-Datei
  output$download_tags <- downloadHandler(
    filename = function() {
      paste0("judo_events_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      write.csv(rv$events, file, row.names = FALSE)
    }
  )

  # Download der Events im speziellen Format für T-Daten
output$download_special_format <- downloadHandler(
  filename = function() {
    paste0("judo_t_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
  },
  content = function(file) {
    req(rv$events)
    if(nrow(rv$events) == 0) {
      return(NULL)
    }
    
    # Erstelle die Formatierung wie gewünscht
    result <- character(0)
    
    # Füge die Headerzeile hinzu
    result <- c(result, "TIME\tEVENT")
    
    # Sortiere die Events nach FPS (Frames)
    events_sorted <- rv$events[order(rv$events$FPS), ]
    
    # Wenn es mindestens ein Event gibt, füge eine Zeile mit ":" ein Frame vor dem ersten Event hinzu
    if (nrow(events_sorted) > 0) {
      first_frame <- events_sorted$FPS[1]
      start_frame <- first_frame - 1
      result <- c(result, paste0(start_frame, "\t:"))
    }
    
    # Verarbeite alle Events
    for (i in 1:nrow(events_sorted)) {
      event <- events_sorted[i, ]
      time_value <- event$FPS
      
      # Sammle alle nicht-NA Werte für den Event-String
      event_values <- c()
      
      # Beginne mit der Rolle (Blau oder Weiss)
      event_values <- c(event_values, event$Rolle)
      
      # Füge alle weiteren nicht-NA Werte hinzu
      for (col in names(event)) {
        # Überspringe Standardspalten und NA-Werte
        if (col %in% c("Zeit", "FPS", "Rolle", "Phase", "Event") || is.na(event[[col]])) {
          next
        }
        event_values <- c(event_values, event[[col]])
      }
      
      # Verbinde alle Werte mit Komma
      event_str <- paste(event_values, collapse = ",")
      
      # Füge die Zeile zum Ergebnis hinzu
      result <- c(result, paste0(time_value, "\t", event_str))
    }
    
    # Füge eine abschließende Zeile mit "&" ein Frame nach dem letzten Event hinzu
    if (nrow(events_sorted) > 0) {
      last_frame <- events_sorted$FPS[nrow(events_sorted)]
      end_frame <- last_frame + 1
      result <- c(result, paste0(end_frame, "\t&"))
    }
    
    # Schreibe das Ergebnis in die Datei
    writeLines(result, file)
  }
)
  # Projektdaten speichern
output$save_project <- downloadHandler(
  filename = function() {
    paste0("judo_project_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
  },
  content = function(file) {
    # Erstelle ein Projektobjekt mit allen relevanten Daten
    project_data <- list(
      events = rv$events,
      current_video = rv$current_video,
      timestamp = Sys.time()
    )
    saveRDS(project_data, file)
  }
)

# Projektdaten laden
observeEvent(input$load_project, {
  req(input$load_project)
  
  # Projektdaten laden
  project_data <- readRDS(input$load_project$datapath)
  
  # Daten wiederherstellen
  rv$events <- project_data$events
  rv$current_video <- project_data$current_video
  
  # Hinweis anzeigen
  showNotification("Projekt erfolgreich geladen", type = "message")
  
  # Video wiederherstellen (wenn möglich)
  if (!is.null(project_data$current_video)) {
    updateSelectInput(session, "selectedVideo", selected = project_data$current_video)
  }
})

# Verbesserte Autosave-Funktionen
autoSaveTimer <- reactiveTimer(30000)  # Reduziert auf 30 Sekunden

# Funktion zum Speichern in mehreren Formaten
saveProjectData <- function() {
  if (nrow(rv$events) > 0) {
    project_data <- list(
      events = rv$events,
      current_video = rv$current_video,
      video_path = if(!is.null(input$videoFile)) input$videoFile$name else NULL,
      timestamp = Sys.time(),
      session_id = session$token
    )
    
    # Speichere in temporärem Ordner mit Zeitstempel
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    temp_file <- file.path(tempdir(), paste0("judo_autosave_", timestamp, ".rds"))
    main_autosave <- file.path(tempdir(), "judo_autosave_current.rds")
    
    # Beide Dateien speichern
    saveRDS(project_data, temp_file)
    saveRDS(project_data, main_autosave)
    
    # CSV-Backup auch erstellen
    csv_file <- file.path(tempdir(), paste0("judo_events_backup_", timestamp, ".csv"))
    write.csv(rv$events, csv_file, row.names = FALSE)
    
    return(TRUE)
  }
  return(FALSE)
}

# Verbessertes automatisches Speichern
observe({
  autoSaveTimer()
  
  result <- saveProjectData()
  
  if (result) {
    session$sendCustomMessage("showAutosaveStatus", 
                              list(text = paste("Gespeichert:", format(Sys.time(), "%H:%M:%S")), 
                                   type = "success"))
  }
})

# Speichere auch bei jeder Änderung der Events
observeEvent(rv$events, {
  # Verzögerte Speicherung um zu häufige Saves zu vermeiden
  invalidateLater(5000, session)  # 5 Sekunden Verzögerung
  
  result <- saveProjectData()
  if (result) {
    session$sendCustomMessage("showAutosaveStatus", 
                              list(text = "Auto-Save", type = "quick"))
  }
}, ignoreInit = TRUE)

# Handler für sofortiges Speichern nach wichtigen Aktionen
observeEvent(input$trigger_immediate_save, {
  result <- saveProjectData()
  if (result) {
    session$sendCustomMessage("showAutosaveStatus", 
                              list(text = "Daten gesichert", type = "success"))
  } else {
    session$sendCustomMessage("showAutosaveStatus", 
                              list(text = "Speichern fehlgeschlagen", type = "error"))
  }
})

# Erweiterte Wiederherstellung
output$restore_autosave <- renderUI({
  temp_dir <- tempdir()
  autosave_files <- list.files(temp_dir, pattern = "judo_autosave.*\\.rds$", full.names = TRUE)
  
  if (length(autosave_files) > 0) {
    # Sortiere nach Änderungszeit (neueste zuerst)
    file_info <- file.info(autosave_files)
    autosave_files <- autosave_files[order(file_info$mtime, decreasing = TRUE)]
    
    div(
      class = "recovery-panel",
      h5("Wiederherstellung"),
      p(paste("Gefunden:", length(autosave_files), "automatische Sicherungen")),
      actionButton("restore_latest", "Neueste Sicherung laden", class = "btn-warning btn-sm"),
      br(), br(),
      selectInput("backup_file", "Oder spezifische Sicherung wählen:", 
                  choices = setNames(autosave_files, 
                                   paste("Sicherung vom", format(file_info$mtime[order(file_info$mtime, decreasing = TRUE)], "%d.%m.%Y %H:%M")))),
      actionButton("restore_selected", "Ausgewählte Sicherung laden", class = "btn-info btn-sm")
    )
  }
})

# Erweiterte Wiederherstellungs-Handler
observeEvent(input$restore_latest, {
  temp_dir <- tempdir()
  current_file <- file.path(temp_dir, "judo_autosave_current.rds")
  
  if (file.exists(current_file)) {
    project_data <- readRDS(current_file)
    rv$events <- project_data$events
    rv$current_video <- project_data$current_video
    
    showNotification(paste("Sicherung wiederhergestellt vom", 
                          format(project_data$timestamp, "%d.%m.%Y %H:%M")), 
                    type = "message")
  }
})

observeEvent(input$restore_selected, {
  req(input$backup_file)
  
  if (file.exists(input$backup_file)) {
    project_data <- readRDS(input$backup_file)
    rv$events <- project_data$events
    rv$current_video <- project_data$current_video
    
    showNotification(paste("Sicherung wiederhergestellt vom", 
                          format(project_data$timestamp, "%d.%m.%Y %H:%M")), 
                    type = "message")
  }
})
}

# App starten
shinyApp(ui, server)
