# Projek.UAS
Projek UAS Kelompok 6
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(agricolae)

ui <- navbarPage("Aplikasi RAL", id = "navbar",
                 tabPanel("1. Tentang RAL",
                          fluidPage(
                            h3("Rancangan Acak Lengkap (RAL)"),
                            p("RAL digunakan saat kondisi percobaan homogen. Perlakuan diberikan secara acak ke unit percobaan."),
                            p("Kegunaan: (1) menyederhanakan rancangan, (2) memudahkan analisis, (3) cocok untuk kondisi seragam."),
                            h4("Struktur Data yang Diperlukan"),
                            tableOutput("contoh_tabel_struktur"),
                            h4("Contoh Tabel Kontingensi (Perlakuan x Ulangan)"),
                            tableOutput("contoh_tabel"),
                            h4("Penjelasan Uji Lanjut"),
                            tags$ul(
                              tags$li(strong("BNT (LSD):"), " Menguji beda nyata antar perlakuan tanpa penyesuaian. Peka tapi cenderung overdeteksi."),
                              tags$li(strong("BNJ (HSD):"), " Gunakan ketika banyak perlakuan. Lebih konservatif daripada BNT."),
                              tags$li(strong("Duncan:"), " Lebih peka dari BNJ, membentuk grup perlakuan yang beda nyata secara bertingkat."),
                              tags$li(strong("SNK:"), " Menguji beda dengan mempertimbangkan jarak urutan rata-rata. Lebih ketat dari Duncan."),
                              tags$li(strong("Scheffe:"), " Paling konservatif, cocok untuk perbandingan kompleks atau data tidak seimbang.")
                            )
                          )
                 ),
                 
                 tabPanel("2. Validasi Data",
                          fluidPage(
                            fileInput("datafile", "Upload file CSV atau Excel", accept = c(".csv", ".xlsx")),
                            numericInput("alpha", "Nilai alpha:", value = 0.05, min = 0.001, max = 0.1, step = 0.001),
                            uiOutput("pilih_kolom"),
                            actionButton("validasi_btn", "Validasi Data"),
                            verbatimTextOutput("validasi_output"),
                            verbatimTextOutput("ringkasan_output"),
                            tableOutput("tabel_kontingensi"),
                            checkboxInput("independen", "Saya menyatakan data ini independen", value = FALSE)
                          )
                 ),
                 
                 tabPanel("3. Uji Hipotesis",
                          fluidPage(
                            uiOutput("ukuran_input"),
                            verbatimTextOutput("hipotesis"),
                            verbatimTextOutput("anova_output"),
                            verbatimTextOutput("keputusan_output"),
                            uiOutput("lanjut_uji")
                          )
                 ),
                 
                 tabPanel("4. Uji Lanjut",
                          fluidPage(
                            uiOutput("lanjut_isi")
                          )
                 )
)

server <- function(input, output, session) {
  rv <- reactiveValues(data=NULL, hasil_anova=NULL, anova_p=NULL, valid=FALSE, kol_perl=NULL, kol_resp=NULL)
  
  observeEvent(input$datafile, {
    ext <- tools::file_ext(input$datafile$name)
    if (ext == "csv") {
      df <- read.csv(input$datafile$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(input$datafile$datapath)
    } else {
      return(NULL)
    }
    rv$data <- df
    output$pilih_kolom <- renderUI({
      req(rv$data)
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
  }, striped = TRUE, bordered = TRUE)
  
  observeEvent(input$validasi_btn, {
    req(rv$data, input$kol_perlakuan, input$kol_respon)
    df <- rv$data
    rv$kol_perl <- input$kol_perlakuan
    rv$kol_resp <- input$kol_respon
    
    perlakuan <- as.factor(df[[rv$kol_perl]])
    respon <- df[[rv$kol_resp]]
    alpha <- input$alpha
    
    if (any(is.na(perlakuan)) || any(is.na(respon))) {
      output$validasi_output <- renderText("❌ Ada nilai hilang. Harap periksa kembali.")
      rv$valid <- FALSE
      return()
    }
    if (!is.numeric(respon)) {
      output$validasi_output <- renderText("❌ Respon harus numerik.")
      rv$valid <- FALSE
      return()
    }
    
    df <- data.frame(perlakuan, respon)
    tab <- table(perlakuan)
    n_perlakuan <- length(tab)
    seimbang <- length(unique(tab)) == 1
    
    norm_res <- by(df$respon, df$perlakuan, function(x) {
      if (length(unique(x)) == 1) return("Tidak bisa diuji")
      pval <- shapiro.test(x)$p.value
      if (pval > alpha) paste0("p = ", round(pval,4), " > ", alpha, " → ✅ Normal")
      else paste0("p = ", round(pval,4), " ≤ ", alpha, " → ⚠️ Tidak Normal")
    })
    
    levene <- leveneTest(respon ~ perlakuan, data = df)
    pval_levene <- levene$`Pr(>F)`[1]
    interpretasi_levene <- ifelse(pval_levene > alpha,
                                  paste0("p = ", round(pval_levene, 4), " > ", alpha, " → ✅ Variansi homogen"),
                                  paste0("p = ", round(pval_levene, 4), " ≤ ", alpha, " → ⚠️ Variansi tidak homogen"))
    
    output$ringkasan_output <- renderPrint({
      cat("Jumlah Perlakuan:", n_perlakuan, "\n")
      print(tab)
      cat("\nNormalitas:\n")
      print(norm_res)
      cat("\nUji Levene:\n")
      print(interpretasi_levene)
    })
    
    if (n_perlakuan < 2) {
      output$validasi_output <- renderText("❌ Harus ada minimal 2 perlakuan.")
      rv$valid <- FALSE
    } else if (!seimbang) {
      output$validasi_output <- renderText("⚠️ Data tidak seimbang. Interpretasi ANOVA lebih hati-hati.")
      rv$valid <- TRUE
    } else {
      output$validasi_output <- renderText("✅ Data valid. Lanjut ke tab Uji Hipotesis.")
      rv$valid <- TRUE
    }
    
    rv$data <- data.frame(perlakuan, respon)
    
    output$tabel_kontingensi <- renderTable({
      df$id <- ave(df$perlakuan, df$perlakuan, FUN=seq_along)
      df_wide <- tidyr::pivot_wider(df, names_from = perlakuan, values_from = respon, values_fill = NA)
      colnames(df_wide)[1] <- "Ulangan ke-"
      df_wide
    }, striped = TRUE, bordered = TRUE)
  })
  
  output$ukuran_input <- renderUI({
    req(rv$valid)
    selectInput("ukuran", "Ukuran Matriks:", choices = paste0(2:6, "x", 2:6))
  })
  
  output$hipotesis <- renderText({
    req(rv$valid)
    "H0: Tidak ada pengaruh perlakuan\nH1: Ada pengaruh perlakuan"
  })
  
  output$anova_output <- renderPrint({
    req(rv$valid)
    hasil <- aov(respon ~ perlakuan, data = rv$data)
    rv$hasil_anova <- hasil
    rv$anova_p <- summary(hasil)[[1]][["Pr(>F)"]][1]
    print(summary(hasil))
  })
  
  output$keputusan_output <- renderPrint({
    req(rv$anova_p)
    if (rv$anova_p <= input$alpha) {
      cat("p =", round(rv$anova_p,4), "≤", input$alpha, "→ Tolak H0: Ada pengaruh perlakuan.")
    } else {
      cat("p =", round(rv$anova_p,4), ">", input$alpha, "→ Gagal tolak H0: Tidak cukup bukti adanya pengaruh perlakuan.")
    }
  })
  
  output$lanjut_uji <- renderUI({
    req(rv$anova_p <= input$alpha)
    radioButtons("uji_lanjut", "Lakukan uji lanjut?", choices = c("Tidak", "Iya"))
  })
  
  output$lanjut_isi <- renderUI({
    req(rv$anova_p)
    if (rv$anova_p > input$alpha) return(HTML("<b>Uji lanjut tidak dilakukan karena H₀ gagal ditolak.</b>"))
    req(input$uji_lanjut)
    if (input$uji_lanjut == "Tidak") {
      verbatimTextOutput("tidak_lanjut")
    } else {
      tagList(
        selectInput("jenis_uji", "Jenis Uji Lanjut:", choices = c("BNT", "BNJ", "Duncan", "SNK", "Scheffe")),
        verbatimTextOutput("uji_lanjut_output")
      )
    }
  })
  
  output$tidak_lanjut <- renderText({ "Tidak dilakukan uji lanjut." })
  
  output$uji_lanjut_output <- renderPrint({
    req(rv$hasil_anova)
    switch(input$jenis_uji,
           "BNT" = print(LSD.test(rv$hasil_anova, "perlakuan", p.adj="none")),
           "BNJ" = print(HSD.test(rv$hasil_anova, "perlakuan")),
           "Duncan" = print(duncan.test(rv$hasil_anova, "perlakuan")),
           "SNK" = print(SNK.test(rv$hasil_anova, "perlakuan")),
           "Scheffe" = print(scheffe.test(rv$hasil_anova, "perlakuan"))
    )
  })
}

shinyApp(ui, server)
