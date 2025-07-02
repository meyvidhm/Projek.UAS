# ===============================
# === LIBRARY
# ===============================
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(agricolae)
library(reshape2)

# ===============================
# === UI
# ===============================
ui <- dashboardPage(
  dashboardHeader(title = "Aplikasi RAL"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Tentang RAL", tabName = "tentang", icon = icon("info-circle")),
      menuItem("2. Validasi Data", tabName = "validasi", icon = icon("check-circle")),
      menuItem("3. Uji Hipotesis", tabName = "uji", icon = icon("flask")),
      menuItem("4. Uji Lanjut", tabName = "lanjut", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap"),
      tags$style(HTML("
        body, h1, h2, h3, h4, h5, p, label {
          font-family: 'Roboto', sans-serif;
        }
      "))
    ),
    
    tabItems(
      # TAB 1: Tentang RAL
      tabItem(tabName = "tentang",
              fluidRow(
                column(width = 6,
                       box(title = "Penjelasan Teori RAL", width = 12, status = "primary", solidHeader = TRUE,
                           h3("Rancangan Acak Lengkap (RAL)"),
                           p("RAL adalah dasar dari berbagai desain percobaan. Konsepnya: setiap unit percobaan diberikan perlakuan secara acak penuh, tanpa pengelompokan."),
                           tags$ul(
                             tags$li(strong("Homogenitas Unit:"), " Semua unit harus seragam."),
                             tags$li(strong("Pengacakan Penuh:"), " Setiap perlakuan diacak ke unit."),
                             tags$li(strong("Satu Sumber Keragaman:"), " Efek perlakuan & galat."),
                             tags$li(strong("Sederhana & Fleksibel:"), " Mudah dirancang dan dianalisis.")
                           ),
                           h4("Tujuan RAL"),
                           tags$ul(
                             tags$li("Melihat pengaruh perlakuan."),
                             tags$li("Membandingkan antar perlakuan."),
                             tags$li("Menarik kesimpulan valid dari hasil eksperimen.")
                           ),
                           h4("Syarat Penting"),
                           tags$ul(
                             tags$li("Unit dan lingkungan homogen."),
                             tags$li("Galat menyebar normal dan homogen."),
                             tags$li("Observasi saling independen.")
                           ),
                           p("Jika kondisi tidak homogen, gunakan RAK.")
                       )
                ),
                column(width = 6,
                       box(title = "Struktur Data yang Diperlukan", width = 12, tableOutput("contoh_tabel_struktur")),
                       box(title = "Contoh Tabel Kontingensi", width = 12, tableOutput("contoh_tabel"))
                )
              ),
              fluidRow(
                box(title = "Penjelasan Uji Lanjut", width = 12, status = "info", solidHeader = TRUE,
                    h4("Uji Lanjut (Post Hoc) Setelah ANOVA"),
                    p("Jika ANOVA signifikan, gunakan uji lanjut untuk mengetahui perlakuan mana yang berbeda."),
                    tags$ul(
                      tags$li(strong("BNT (LSD):"), " sensitif, cocok untuk perlakuan sedikit."),
                      tags$li(strong("BNJ (HSD Tukey):"), " konservatif, cocok banyak perlakuan."),
                      tags$li(strong("Duncan:"), " membentuk grup berbeda bertingkat."),
                      tags$li(strong("SNK:"), " berdasarkan urutan rata-rata."),
                      tags$li(strong("Scheffe:"), " paling konservatif, untuk perbandingan kompleks.")
                    )
                )
              ),
              fluidRow(
                box(title = "Rumus ANOVA & Uji Lanjut", width = 12, status = "warning", solidHeader = TRUE,
                    withMathJax(HTML("
              <b>Model:</b><br>
              $$ Y_{ij} = \\mu + \\tau_i + \\epsilon_{ij} $$
              <b>Komponen Ragam:</b><br>
              $$ SST = \\sum (Y_{ij} - \\bar{Y}_{..})^2 $$
              $$ SSA = r \\sum (\\bar{Y}_{i.} - \\bar{Y}_{..})^2 $$
              $$ SSE = SST - SSA $$
              $$ F = \\frac{SSA/(a-1)}{SSE/(a(r-1))} $$
              <b>Uji Lanjut:</b><br>
              $$ BNT = t_{\\alpha/2, df} \\cdot \\sqrt{\\frac{2 \\cdot MSE}{r}} $$<br>
              $$ HSD = q_{\\alpha, a, df} \\cdot \\sqrt{\\frac{MSE}{r}} $$<br>
              $$ D_{ij} = r_{ij} \\cdot SE \\quad (Duncan) $$<br>
              $$ SNK = q_{ij} \\cdot SE $$<br>
              $$ S = (a - 1) \\cdot F_{\\alpha, a-1, df} \\cdot \\frac{MSE}{r} \\quad (Scheffe) $$
            "))
                )
              )
      ),
      
      # TAB 2: Validasi
      tabItem(tabName = "validasi",
              fluidRow(
                box(title = "Upload & Input", width = 4, status = "info",
                    fileInput("datafile", "Upload file CSV atau Excel", accept = c(".csv", ".xlsx")),
                    numericInput("alpha", "Nilai alpha:", 0.05, 0.001, 0.1, 0.001),
                    uiOutput("pilih_kolom"),
                    actionButton("validasi_btn", "Validasi Data"),
                    checkboxInput("independen", "Saya menyatakan data ini independen", FALSE)
                ),
                box(title = "Hasil Validasi", width = 8,
                    verbatimTextOutput("validasi_output"),
                    verbatimTextOutput("ringkasan_output"),
                    tableOutput("tabel_kontingensi"),
                    plotOutput("boxplot_data")
                )
              )
      ),
      
      # TAB 3: Uji ANOVA
      tabItem(tabName = "uji",
              fluidRow(
                box(title = "Uji ANOVA", width = 6, status = "primary",
                    uiOutput("ukuran_input"),
                    verbatimTextOutput("hipotesis"),
                    verbatimTextOutput("anova_output"),
                    verbatimTextOutput("keputusan_output")
                ),
                box(title = "Lanjut ke Uji Lanjut?", width = 6,
                    uiOutput("lanjut_uji")
                )
              ),
              fluidRow(
                box(title = "Interpretasi Hasil ANOVA", width = 12, status = "success", solidHeader = TRUE,
                    htmlOutput("interpretasi_anova")
                )
              )
      ),  # ✅ ← koma yang diperlukan
      
      # TAB 4: Uji Lanjut
      tabItem(tabName = "lanjut",
              fluidRow(
                box(title = "Hasil Uji Lanjut", width = 12,
                    uiOutput("lanjut_isi")
                )
              )
      )
    )
  )
)

# ===============================
# === SERVER
# ===============================
server <- function(input, output, session) {
  rv <- reactiveValues(data=NULL, hasil_anova=NULL, anova_p=NULL, valid=FALSE)
  
  observeEvent(input$datafile, {
    ext <- tools::file_ext(input$datafile$name)
    df <- if (ext == "csv") read.csv(input$datafile$datapath) else read_excel(input$datafile$datapath)
    rv$data <- df
    output$pilih_kolom <- renderUI({
      tagList(
        selectInput("kol_perlakuan", "Kolom Perlakuan:", choices = names(rv$data)),
        selectInput("kol_respon", "Kolom Respon:", choices = names(rv$data))
      )
    })
  })
  
  output$contoh_tabel_struktur <- renderTable({
    data.frame(
      Perlakuan = c(rep("Pupuk A", 3), rep("Pupuk B", 3), rep("Pupuk C", 3)),
      Tinggi_Tanaman = c(15.2, 14.8, 15.5, 17.1, 16.9, 17.5, 12.5, 12.8, 12.0)
    )
  })
  
  output$contoh_tabel <- renderTable({
    data.frame(
      `Ulangan ke-` = 1:3,
      `Pupuk A` = c(15.2, 14.8, 15.5),
      `Pupuk B` = c(17.1, 16.9, 17.5),
      `Pupuk C` = c(12.5, 12.8, 12.0)
    )
  })
  
  observeEvent(input$validasi_btn, {
    req(rv$data, input$kol_perlakuan, input$kol_respon)
    if (input$kol_perlakuan == input$kol_respon || !is.numeric(rv$data[[input$kol_respon]])) {
      output$validasi_output <- renderText("❌ Respon harus numerik.")
      output$ringkasan_output <- renderPrint({})
      output$tabel_kontingensi <- renderTable({})
      output$boxplot_data <- renderPlot({})
      rv$valid <- FALSE
      return()
    }
    
    df <- data.frame(
      perlakuan = as.factor(rv$data[[input$kol_perlakuan]]),
      respon = rv$data[[input$kol_respon]]
    )
    rv$data <- df
    alpha <- input$alpha
    norm_res <- by(df$respon, df$perlakuan, function(x) {
      if (length(unique(x)) == 1) return("Tidak bisa diuji")
      p <- shapiro.test(x)$p.value
      if (p > alpha) paste0("p = ", round(p, 4), " > ", alpha, " → ✅ Normal")
      else paste0("p = ", round(p, 4), " ≤ ", alpha, " → ⚠️ Tidak Normal")
    })
    levene <- leveneTest(respon ~ perlakuan, data = df)
    
    output$ringkasan_output <- renderPrint({
      print(table(df$perlakuan))
      print(norm_res)
      cat("Levene Test: p =", round(levene$`Pr(>F)`[1], 4))
    })
    
    output$tabel_kontingensi <- renderTable({
      df %>% group_by(perlakuan) %>% mutate(Ulangan = row_number()) %>%
        dcast(Ulangan ~ perlakuan, value.var = "respon")
    })
    
    output$boxplot_data <- renderPlot({
      boxplot(respon ~ perlakuan, data = df, col = "lightblue", main = "Boxplot Respon per Perlakuan",
              xlab = "Perlakuan", ylab = "Respon")
    })
    
    rv$valid <- TRUE
    output$validasi_output <- renderText("✅ Data valid. Lanjut ke tab Uji Hipotesis.")
  })
  
  output$ukuran_input <- renderUI({
    req(rv$valid)
    selectInput("ukuran", "Ukuran Matriks:", choices = paste0(2:6, "x", 2:6))
  })
  
  output$hipotesis <- renderText({ "H0: Tidak ada pengaruh perlakuan\nH1: Ada pengaruh perlakuan" })
  
  output$anova_output <- renderPrint({
    hasil <- aov(respon ~ perlakuan, data = rv$data)
    rv$hasil_anova <- hasil
    
    anova_table <- summary(hasil)[[1]]
    rv$anova_p <- anova_table[["Pr(>F)"]][1]
    rv$anova_f <- anova_table[["F value"]][1]
    
    cat("📊 Tabel ANOVA:\n")
    print(anova_table)
    
    cat("\n📌 Nilai Statistik:\n")
    cat("F-hitung :", round(rv$anova_f, 4), "\n")
    cat("p-value  :", round(rv$anova_p, 5), "\n")
  })
  
  
  output$keputusan_output <- renderPrint({
    if (rv$anova_p <= input$alpha) cat("Tolak H0: Ada pengaruh perlakuan.")
    else cat("Gagal tolak H0: Tidak ada pengaruh perlakuan.")
  })
  
  output$interpretasi_anova <- renderUI({
    req(rv$anova_p)
    
    if (rv$anova_p <= input$alpha) {
      HTML(paste0("
      <div style='padding:12px; background:#e3fcec; border-left:5px solid #2ecc71'>
        <h4><b>✅ Kesimpulan:</b></h4>
        <p>Terdapat <b>perbedaan yang signifikan</b> antara perlakuan (<i>p-value = ", round(rv$anova_p, 5), " < ", input$alpha, "</i>).</p>

        <h4><b>📌 Interpretasi:</b></h4>
        <p>Perlakuan yang diberikan <b>memiliki pengaruh nyata</b> terhadap nilai respon. Artinya, nilai rata-rata antar kelompok perlakuan tidak bisa dianggap sama, karena variasinya terlalu besar untuk dianggap sebagai kebetulan.</p>

        <h4><b>💡 Saran:</b></h4>
        <p>Lanjutkan ke <b>Uji Lanjut</b> untuk mengetahui pasangan perlakuan mana yang berbeda secara signifikan.</p>
      </div>
    "))
    } else {
      HTML(paste0("
      <div style='padding:12px; background:#fdecea; border-left:5px solid #e74c3c'>
        <h4><b>🚫 Kesimpulan:</b></h4>
        <p>Tidak ditemukan perbedaan yang signifikan antar perlakuan (<i>p-value = ", round(rv$anova_p, 5), " ≥ ", input$alpha, "</i>).</p>

        <h4><b>📌 Interpretasi:</b></h4>
        <p>Perlakuan yang diberikan <b>tidak terbukti mempengaruhi</b> nilai respon secara statistik. Hasil respon antar perlakuan dianggap setara.</p>

        <h4><b>💡 Saran:</b></h4>
        <p>Tinjau kembali desain eksperimen atau coba uji pada variabel respon yang berbeda.</p>
      </div>
    "))
    }
  })
  
  output$lanjut_uji <- renderUI({
    req(rv$anova_p <= input$alpha)
    radioButtons("uji_lanjut", "Lakukan uji lanjut?", c("Tidak", "Ya"))
  })
  
  output$lanjut_isi <- renderUI({
    req(rv$hasil_anova, rv$anova_p <= input$alpha)
    if (input$uji_lanjut == "Ya") {
      tagList(
        selectInput("jenis_uji", "Jenis Uji Lanjut:", choices = c("BNT", "BNJ", "Duncan", "SNK", "Scheffe")),
        verbatimTextOutput("uji_lanjut_output")
      )
    } else {
      h4("Anda tidak memilih untuk uji lanjut.")
    }
  })
  
  observeEvent(input$jenis_uji, {
    req(rv$hasil_anova, input$jenis_uji)
    hasil <- switch(input$jenis_uji,
                    "BNT" = LSD.test(rv$hasil_anova, "perlakuan", p.adj = "none"),
                    "BNJ" = HSD.test(rv$hasil_anova, "perlakuan"),
                    "Duncan" = duncan.test(rv$hasil_anova, "perlakuan"),
                    "SNK" = SNK.test(rv$hasil_anova, "perlakuan"),
                    "Scheffe" = scheffe.test(rv$hasil_anova, "perlakuan"))
    output$uji_lanjut_output <- renderPrint({ hasil })
  })
}

# ===============================
# === APP
# ===============================
shinyApp(ui, server)

