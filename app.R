#
# Statistik Trainer - Shiny App
#

library(shiny)
library(MASS)
library(ggplot2)
library(DT)
library(effsize)
library(cowplot)

my_green <- "#00C1BF"

# Task definitions ---------------------------------------------------------

aufgabe <- data.frame(
  task = c("xtable", "modalwert", "mean", "median", "spannweite", "var", "sd", "iqr", "boxplot",
           "binom", "bernoulli", "scale", "zdist",
           "confint_mean_with_var", "confint_mean_no_var",
           "cov", "cor_pearson", "cor_spearman", "cor_sig", "cor_sig2", "cor_sig_s", "cor_sig_s2", "t.test1", "t.test1_2", "t.test", "t.test2",
           "abh_t.test", "abh_t.test2", "oneway_anova", "rm_anova",
           "chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld",
           "alpha_kumulierung", "bonferroni", "fdr", "cohend", "eta2", "omega2", "cramersv", "phi", "lm", "lm_predict", "lm_std", "lm_sig", "lm_sig2"),
  label = c("Kreuztabelle", "Modalwert", "Mittelwert", "Median", "Spannweite", "Varianz",
            "Standardabweichung", "Quartilsabstand", "Boxplot",
            "Binomialverteilung", "Bernoulliverteilung", "z-Transformation",
            "Standardnormalverteilung",
            "KI f\u00fcr Mittelwert, \u03c3 bekannt",
            "KI f\u00fcr Mittelwert, \u03c3 unbekannt",
            "Kovarianz", "Pearson-Korrelation",
            "Spearman-Korrelation",
            "Pearson-Korrelation (Signifikanztest, zweiseitig)",
            "Pearson-Korrelation (Signifikanztest, einseitig)",
            "Spearman-Korrelation (Signifikanztest, zweiseitig)",
            "Spearman-Korrelation (Signifikanztest, einseitig)",
            "Ein-Stichproben t-Test (zweiseitig)", "Ein-Stichproben t-Test (einseitig)",
            "Unabh. t-Test (zweiseitig)",
            "Unabh. t-Test (einseitig)", "Abh. t-Test (zweiseitig)",
            "Abh. t-Test (einseitig)", "Einfaktorielle ANOVA",
            "ANOVA mit Messwiederholung",
            "\u03c7\u00b2-Anpassungstest (Gleichverteilung)",
            "\u03c7\u00b2-Anpassungstest (Verh\u00e4ltnis)",
            "\u03c7\u00b2-Unabh\u00e4ngigkeitstest",
            "Vier-Felder \u03c7\u00b2-Test",
            "\u03b1-Fehler Kumulierung",
            "Bonferroni-Korrektur",
            "FDR-Korrektur (Benjamini-Hochberg)",
            "Effektst\u00e4rke: Cohen's d",
            "Effektst\u00e4rke: \u03b7\u00b2",
            "Effektst\u00e4rke: \u03c9\u00b2",
            "Effektst\u00e4rke: Cram\u00e9rs V",
            "Effektst\u00e4rke: \u03c6 (Phi)",
            "Lineare Regression",
            "Lineare Regression (Vorhersage)",
            "Standardisierung Regressionskoeffizient",
            "Signifikanztest Regressionskoeffizient (zweiseitig)",
            "Signifikanztest Regressionskoeffizient (einseitig)"),
  ask = c(
    "Aufgabe: Erstellen Sie eine Kreuztabelle von Variable \\(X\\) (Merkmale \\(X_1\\) und \\(X_2\\)) f\u00fcr die beiden Gruppen A und B.",
    "Aufgabe: Bestimmen Sie den Modalwert (\\(x_{mod}\\)) der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie den Mittelwert (\\(\\bar{x}\\)) der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie den Median der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie die Spannweite (\\(R\\)) der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie die Varianz (\\(s^2\\)) der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie die Standardabweichung (\\(s\\)) der Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie den Quartilsabstand (IQR) f\u00fcr die Variable (\\(X\\)):",
    "Aufgabe: Berechnen Sie f\u00fcr die Variable (\\(X\\)) Median, unteres Quartil (\\(\\tilde{x}_{0.25}\\)), oberes Quartil (\\(\\tilde{x}_{0.75}\\)) sowie die Whisker-Enden und stellen Sie die Verteilung als Boxplot dar.",
    "",
    "",
    "Aufgabe: Standardisieren Sie die Variable (\\(X\\)):",
    "Aufgabe: Bestimmen Sie f\u00fcr die Auspr\u00e4gungen der Variable \\(X\\) die Wahrscheinlichkeit, jeweils diesen oder einen kleineren Wert zu erhalten. Die Wahrscheinlichkeiten k\u00f6nnen aus der z-Tabelle abgelesen werden:",
    "Aufgabe: Berechnen Sie das 95% Konfidenzintervall f\u00fcr den Mittelwert der folgenden Variable (\\(X\\)). Die Populationsvarianz ist bekannt.",
    "Aufgabe: Berechnen Sie das 95% Konfidenzintervall f\u00fcr den Mittelwert der folgenden Variable (\\(X\\)). Die Populationsvarianz ist unbekannt.",
    "Aufgabe: Berechnen Sie die Kovarianz (\\(s_{xy}\\)) zwischen \\(X\\) und \\(Y\\).",
    "Aufgabe: Berechnen Sie die Pearson-Korrelation (\\(r\\)) zwischen \\(X\\) und \\(Y\\).",
    "Aufgabe: Berechnen Sie die Spearman-Korrelation (\\(r_s\\)) zwischen \\(X\\) und \\(Y\\).",
    "Aufgabe: Gegeben ist der folgende Pearson-Korrelationskoeffizient sowie die zugeh\u00f6rigen Daten. Testen Sie zweiseitig, ob der Korrelationskoeffizient \\(r\\) signifikant von 0 verschieden ist (\\(\\alpha = .05\\)).",
    "Aufgabe: Gegeben ist der folgende Pearson-Korrelationskoeffizient sowie die zugeh\u00f6rigen Daten. Testen Sie einseitig, ob der Korrelationskoeffizient \\(r\\) signifikant gr\u00f6\u00dfer als 0 ist (\\(\\alpha = .05\\)).",
    "Aufgabe: Gegeben ist der folgende Spearman-Korrelationskoeffizient sowie die zugeh\u00f6rigen Daten. Testen Sie zweiseitig, ob der Korrelationskoeffizient \\(r_s\\) signifikant von 0 verschieden ist (\\(\\alpha = .05\\)).",
    "Aufgabe: Gegeben ist der folgende Spearman-Korrelationskoeffizient sowie die zugeh\u00f6rigen Daten. Testen Sie einseitig, ob der Korrelationskoeffizient \\(r_s\\) signifikant gr\u00f6\u00dfer als 0 ist (\\(\\alpha = .05\\)).",
    "",
    "",
    "Aufgabe: Berechnen Sie einen zweiseitigen unabh\u00e4ngigen t-Test f\u00fcr den Mittelwertsunterschied zwischen den Gruppen A und B (Varianzhomogenit\u00e4t darf angenommen werden, \\(\\alpha = .05\\)).",
    "Aufgabe: Berechnen Sie einen einseitigen unabh\u00e4ngigen t-Test f\u00fcr den Mittelwertsunterschied zwischen den Gruppen A und B (Varianzhomogenit\u00e4t darf angenommen werden, \\(\\alpha = .05\\)).",
    "Aufgabe: Berechnen Sie einen zweiseitigen abh\u00e4ngigen t-Test f\u00fcr den Mittelwertsunterschied zwischen den Messzeitpunkten \\(t_0\\) (Pr\u00e4) und \\(t_1\\) (Post) (\\(\\alpha = .05\\)).",
    "Aufgabe: Berechnen Sie einen einseitigen abh\u00e4ngigen t-Test f\u00fcr den Mittelwertsunterschied zwischen den Messzeitpunkten \\(t_0\\) (Pr\u00e4) und \\(t_1\\) (Post) (\\(\\alpha = .05\\)).",
    "Aufgabe: Berechnen Sie eine einfaktorielle ANOVA f\u00fcr den Mittelwertsunterschied zwischen den Gruppen A, B und C (Varianzhomogenit\u00e4t darf angenommen werden, \\(\\alpha = .05\\)).",
    "Aufgabe: Berechnen Sie eine ANOVA mit Messwiederholung f\u00fcr den Mittelwertsunterschied zwischen den Messzeitpunkten \\(t_0\\), \\(t_1\\) und \\(t_2\\) (\\(\\alpha = .05\\)).",
    "Aufgabe: Testen Sie, ob die H\u00e4ufigkeitsverteilung der Variable X einer Gleichverteilung entspricht (\\(\\alpha = .05\\)).",
    "",
    "Aufgabe: Testen Sie, ob ein Zusammenhang zwischen den Variablen X und Y besteht (\\(\\alpha = .05\\)).",
    "Aufgabe: F\u00fchren Sie einen Vier-Felder Chi-Quadrat-Test durch und testen Sie, ob ein Zusammenhang zwischen den Variablen X und Y besteht (\\(\\alpha = .05\\)).",
    "",
    "",
    "",
    "Aufgabe: Berechnen Sie Cohen's d f\u00fcr den Mittelwertsunterschied zwischen den Gruppen A und B.",
    "",
    "",
    "",
    "",
    "Aufgabe: Stellen Sie die Geradengleichung f\u00fcr den linearen Zusammenhang zwischen \\(X\\) und \\(Y\\) auf.",
    "Aufgabe: Gegeben ist das folgende Regressionsmodell. Berechnen Sie den vorhergesagten Wert \\(\\hat{y}\\) f\u00fcr den angegebenen Wert von \\(x\\):",
    "Aufgabe: Gegeben ist das folgende Regressionsmodell sowie die zugeh\u00f6rigen Daten. Standardisieren Sie den Regressionskoeffizienten \\(b\\).",
    "Aufgabe: Gegeben ist das folgende Regressionsmodell sowie die zugeh\u00f6rigen Daten. Testen Sie zweiseitig, ob der Regressionskoeffizient \\(b\\) signifikant von 0 verschieden ist (\\(\\alpha = .05\\)).",
    "Aufgabe: Gegeben ist das folgende Regressionsmodell sowie die zugeh\u00f6rigen Daten. Testen Sie einseitig, ob der Regressionskoeffizient \\(b\\) signifikant gr\u00f6\u00dfer als 0 ist (\\(\\alpha = .05\\))."
  ),
  formel = c(
    "",
    "Formel: Der Modalwert \\(x_{mod}\\) ist der Wert mit der h\u00f6chsten H\u00e4ufigkeit in der Verteilung.",
    "Formel: $$ \\bar{x}=\\frac{\\sum\\limits _{i=1}^{n}x_{i}}{n} $$",
    "Formel: $$ \\def\\arraystretch{1.2} \\textit{Md} = \\Bigg\\{\\begin{array}{@{}c@{}}\\frac{x_{(\\frac{n}{2})}+x_{(\\frac{n}{2}+1)}}{2} \\quad \\textrm{falls }n \\textrm{ gerade}\\\\[6pt] x_{(\\frac{n+1}{2})}\\quad \\textrm{falls }n \\textrm{ ungerade}\\end{array}$$",
    "Formel: $$ R = x_{max} - x_{min} $$",
    "Formel: $$ s^2=\\dfrac{\\displaystyle\\sum_{i=1}^{n}(x_{i}-\\bar{x})^2}{n-1} $$",
    "Formel: $$ s=\\sqrt{\\frac{\\displaystyle\\sum_{i=1}^{n}(x_{i}-\\bar{x})^2}{n-1}} $$",
    "Formel: $$ \\tilde{x}_{\\alpha} = \\begin{cases} x_{(l)} & \\text{falls } n \\cdot \\alpha \\text{ keine ganze Zahl ist, } l = \\text{die auf } n \\cdot \\alpha \\text{ folgende ganze Zahl} \\\\ \\frac{x_{(l)} + x_{(l+1)}}{2} & \\text{falls } n \\cdot \\alpha \\text{ eine ganze Zahl ist, } l = n \\cdot \\alpha \\end{cases} $$",
    "Formel: $$ \\def\\arraystretch{1.2} \\textit{Md} = \\Bigg\\{\\begin{array}{@{}c@{}}\\frac{x_{(\\frac{n}{2})}+x_{(\\frac{n}{2}+1)}}{2} \\quad \\textrm{falls }n \\textrm{ gerade}\\\\[6pt] x_{(\\frac{n+1}{2})}\\quad \\textrm{falls }n \\textrm{ ungerade}\\end{array}$$ \n $$ \\tilde{x}_{\\alpha} = \\begin{cases} x_{(l)} & \\text{falls } n \\cdot \\alpha \\text{ keine ganze Zahl ist, } l = \\text{die auf } n \\cdot \\alpha \\text{ folgende ganze Zahl} \\\\ \\frac{x_{(l)} + x_{(l+1)}}{2} & \\text{falls } n \\cdot \\alpha \\text{ eine ganze Zahl ist, } l = n \\cdot \\alpha \\end{cases} $$ \n $$ \\text{Whisker: bis zum letzten Wert innerhalb von } \\tilde{x}_{0.25/0.75} \\pm 1.5 \\cdot IQR $$",
    "Formel: $$ P(X = k) = \\binom{n}{k} \\cdot p^k \\cdot (1-p)^{n-k} = \\frac{n!}{k! \\cdot (n-k)!} \\cdot p^k \\cdot (1-p)^{n-k} $$",
    "Formel: $$ P(X = k) = p^k \\cdot (1-p)^{1-k}, \\quad k \\in \\{0, 1\\} $$",
    "Formel: $$ z_i=\\frac{x_i-\\bar{x}}{s} $$",
    "Formel: $$ z_i=\\frac{x_i-\\bar{x}}{s} $$",
    "Formel: $$ \\mu_{1,2}=\\bar{x} \\pm z_{1-\\frac{\\alpha}{2}}\\frac{\\sigma}{\\sqrt{n}}$$",
    "Formel: $$ \\mu_{1,2}=\\bar{x} \\pm t_{1-\\frac{\\alpha}{2}}\\frac{\\hat{\\sigma}}{\\sqrt{n}}, df=n-1$$",
    "Formel: $$ s_{xy}=\\frac{\\displaystyle \\sum_{i=1}^{n}(x_{i}-\\bar{x})\\cdot(y_{i}-\\bar{y})}{n-1} $$",
    "Formel: $$ r=\\frac{s_{xy}}{s_x\\cdot s_y} $$",
    "Formel: $$ r_s={}1-\\frac{6 \\cdot \\displaystyle \\sum_{i=1}^{n}d^2_i}{N \\cdot (N^2 - 1)} $$",
    "Formel: $$ t = \\frac{r \\cdot \\sqrt{N-2}}{\\sqrt{1-r^2}}; \\quad df = N-2 $$",
    "Formel: $$ t = \\frac{r \\cdot \\sqrt{N-2}}{\\sqrt{1-r^2}}; \\quad df = N-2 $$",
    "Formel: $$ t = \\frac{r_s \\cdot \\sqrt{N-2}}{\\sqrt{1-r_s^2}}; \\quad df = N-2 $$",
    "Formel: $$ t = \\frac{r_s \\cdot \\sqrt{N-2}}{\\sqrt{1-r_s^2}}; \\quad df = N-2 $$",
    "Formel: $$ t = \\frac{\\bar{x} - \\mu_0}{\\frac{s}{\\sqrt{n}}}; \\quad df = n-1 $$",
    "Formel: $$ t = \\frac{\\bar{x} - \\mu_0}{\\frac{s}{\\sqrt{n}}}; \\quad df = n-1 $$",
    "Formel: $$ t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{(n_1 - 1) \\cdot \\sigma_1^2 + (n_2 - 1) \\cdot \\sigma_2^2}{(n_1 - 1) + (n_2 - 1)} \\cdot \\left(\\frac{1}{n_1} + \\frac{1}{n_2} \\right)}}; df = n_1 + n_2 - 2 $$",
    "Formel: $$ t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{(n_1 - 1) \\cdot \\sigma_1^2 + (n_2 - 1) \\cdot \\sigma_2^2}{(n_1 - 1) + (n_2 - 1)} \\cdot \\left(\\frac{1}{n_1} + \\frac{1}{n_2} \\right)}}; df = n_1 + n_2 - 2 $$",
    "Formel: $$ t=\\frac{\\bar{x}_d}{\\hat{\\sigma}_{\\bar{x}_d}} $$",
    "Formel: $$ t=\\frac{\\bar{x}_d}{\\hat{\\sigma}_{\\bar{x}_d}} $$",
    "Formel: $$ F_{(df1,df2)} = \\frac{\\sigma^2_{Sys}}{\\sigma^2_{Res}} $$ \n $$ df_1 = p-1 $$ \n $$ df_2 = p \\cdot (n-1) $$",
    "Formel: $$ F = \\frac{\\hat{\\sigma}_A^2}{\\hat{\\sigma}_{A \\times Vpn}^2} $$ \n $$ \\hat{\\sigma}_A^2 = \\frac{n \\cdot \\sum(\\bar{A}_i - \\bar{G})^2}{p-1} $$ \n $$ \\hat{\\sigma}_{A \\times Vpn}^2 = \\frac{\\sum_i \\sum_m [x_{im}-(\\bar{A}_i+\\bar{P}_m-\\bar{G})]^2}{(p-1)(n-1)} $$",
    "Formel: $$ \\chi^2 = \\sum\\limits_{i=1}^{k} \\frac{(f_{bi} - f_{ei})^2}{f_{ei}}, \\quad df = k-1 $$",
    "Formel: $$ \\chi^2 = \\sum\\limits_{i=1}^{k} \\frac{(f_{bi} - f_{ei})^2}{f_{ei}}, \\quad df = k-1 $$",
    "Formel: $$ \\chi^2 = \\sum\\limits_{i=1}^{k}\\sum\\limits_{j=1}^{l} \\frac{(f_{bij} - f_{eij})^2}{f_{eij}}, \\quad df = (k-1) \\cdot (l-1) $$",
    "Formel: $$ \\chi^2 = \\frac{N \\cdot (a \\cdot d - b \\cdot c)^2}{(a+b) \\cdot (c+d) \\cdot (a+c) \\cdot (b+d)}, \\quad df = 1 $$",
    "Formel: $$ P(\\text{mind. 1 Fehler 1. Art}) = 1 - (1 - \\alpha)^k $$",
    "Formel: $$ \\alpha^* = \\frac{\\alpha}{k} $$",
    "Formel: $$ p^*_{(i)} = p_{(i)} \\cdot \\frac{k}{i}, \\quad i = 1, \\ldots, k $$",
    "Formel: $$ d=\\frac{\\bar{x}_{A}-\\bar{x}_{B}}{\\sqrt{\\frac{s_A^2+s^2_B}{2}}} $$",
    "Formel: $$ \\eta^2 = \\frac{df_1 \\cdot F}{df_1 \\cdot F + df_2} $$",
    "Formel: $$ \\omega^2 = \\frac{df_1 \\cdot (F - 1)}{df_1 \\cdot (F - 1) + N} $$",
    "Formel: $$ V = \\sqrt{\\frac{\\chi^2}{N \\cdot (k-1)}} $$",
    "Formel: $$ \\phi = \\sqrt{\\frac{\\chi^2}{N}} $$",
    "Formel: $$ y_{i}= a + b\\cdot x_{i}$$ \n $$ b=\\frac{s_{xy}}{s^2_x}$$ \n $$ a = \\bar{y} - b \\cdot \\bar{x}$$",
    "Formel: $$ \\hat{y} = a + b \\cdot x $$",
    "Formel: $$ \\beta = b \\cdot \\frac{s_x}{s_y} $$",
    "Formel: $$ \\hat{\\sigma}_{(y|x)} = \\sqrt{\\frac{n \\cdot s_y^2 - n \\cdot b^2 \\cdot s_x^2}{n-2}} \\quad s_b = \\frac{\\hat{\\sigma}_{(y|x)}}{s_x \\cdot \\sqrt{n}} \\quad t_{emp} = \\frac{b}{s_b}, \\quad df = n-2 $$",
    "Formel: $$ \\hat{\\sigma}_{(y|x)} = \\sqrt{\\frac{n \\cdot s_y^2 - n \\cdot b^2 \\cdot s_x^2}{n-2}} \\quad s_b = \\frac{\\hat{\\sigma}_{(y|x)}}{s_x \\cdot \\sqrt{n}} \\quad t_{emp} = \\frac{b}{s_b}, \\quad df = n-2 $$"
  ),
  stringsAsFactors = FALSE
)

# Value spaces -------------------------------------------------------------

means_vals  <- 1:20
ns_vals     <- c(10, 12)    # even values only; 12 is also divisible by 3
sds_vals    <- seq(0.1, 2, .1)
cor_vals    <- seq(-1, 1, .01)
cohend_vals <- seq(0, 1.5, .01)

# LaTeX helper functions ---------------------------------------------------

latex_mean_steps <- function(x, n) {
  paste(
    "$$\\bar{x}=", "\\frac{", paste(x, collapse = " + "), "}{", n, "}", "$$",
    "\n",
    "$$\\bar{x}=", round(mean(x), 2), "$$"
  )
}

latex_var_steps <- function(x, n) {
  xbar <- round(mean(x), 2)
  paste(
    "$$ s^2=\\dfrac{\\displaystyle", paste(paste0("(", x, "-", xbar, ")^2"), collapse = " + "), "}{", n, "-1} $$",
    "\n",
    "$$ s^2=\\dfrac{\\displaystyle", paste(round((x - mean(x))^2, 2), collapse = " + "), "}{", n, "-1} $$"
  )
}

latex_sd_result <- function(x) {
  paste(
    "$$ s^2=", round(var(x), 2), "$$",
    "\n",
    "$$ s=\\sqrt{", round(var(x), 2), "}$$"
  )
}

interpret_eta2 <- function(e) {
  label <- if (e < 0.01) "einem vernachl\u00e4ssigbaren Effekt" else
            if (e < 0.06) "einem kleinen Effekt" else
            if (e < 0.14) "einem mittleren Effekt" else
                          "einem gro\u00dfen Effekt"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1988) entspricht dies ", label, " (",
         "\\(\\eta^2 < .01\\) vernachl\u00e4ssigbar, \\(\\eta^2 < .06\\) klein, \\(\\eta^2 < .14\\) mittel, \\(\\eta^2 \\geq .14\\) gro\u00df).")
}

interpret_omega2 <- function(o) {
  label <- if (o < 0.01) "einem vernachl\u00e4ssigbaren Effekt" else
            if (o < 0.06) "einem kleinen Effekt" else
            if (o < 0.14) "einem mittleren Effekt" else
                          "einem gro\u00dfen Effekt"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1988) entspricht dies ", label, " (",
         "\\(\\omega^2 < .01\\) vernachl\u00e4ssigbar, \\(\\omega^2 < .06\\) klein, \\(\\omega^2 < .14\\) mittel, \\(\\omega^2 \\geq .14\\) gro\u00df).")
}

interpret_phi <- function(p) {
  label <- if (p < 0.1) "einem vernachl\u00e4ssigbaren Effekt" else
            if (p < 0.3) "einem kleinen Effekt" else
            if (p < 0.5) "einem mittleren Effekt" else
                         "einem gro\u00dfen Effekt"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1988) entspricht dies ", label, " (",
         "\\(\\phi < .10\\) vernachl\u00e4ssigbar, \\(\\phi < .30\\) klein, \\(\\phi < .50\\) mittel, \\(\\phi \\geq .50\\) gro\u00df).")
}

interpret_cramersv <- function(v) {
  label <- if (v < 0.1) "einem vernachl\u00e4ssigbaren Effekt" else
            if (v < 0.3) "einem kleinen Effekt" else
            if (v < 0.5) "einem mittleren Effekt" else
                         "einem gro\u00dfen Effekt"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1988) entspricht dies ", label, " (",
         "\\(V < .10\\) vernachl\u00e4ssigbar, \\(V < .30\\) klein, \\(V < .50\\) mittel, \\(V \\geq .50\\) gro\u00df).")
}

interpret_cohend <- function(d) {
  label <- if (abs(d) < 0.2) "einem vernachl\u00e4ssigbaren Effekt" else
            if (abs(d) < 0.5) "einem kleinen Effekt" else
            if (abs(d) < 0.8) "einem mittleren Effekt" else
                               "einem gro\u00dfen Effekt"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1992) entspricht dies ", label, ".")
}

interpret_cor <- function(r) {
  label <- if (abs(r) < 0.1) "einem vernachl\u00e4ssigbaren Zusammenhang" else
            if (abs(r) < 0.3) "einem schwachen Zusammenhang" else
            if (abs(r) < 0.5) "einem mittleren Zusammenhang" else
                               "einem starken Zusammenhang"
  paste0("<br><strong>Interpretation:</strong> Nach Cohen (1992) entspricht dies ", label, ".")
}

latex_onesample_ttest_loesungsweg <- function(x, mu0, two_sided) {
  n      <- length(x)
  p_crit <- if (two_sided) 0.975 else 0.95
  df     <- n - 1
  xbar   <- round(mean(x), 2)
  s      <- round(sqrt(var(x)), 2)
  se     <- round(s / sqrt(n), 2)
  t_emp  <- round((xbar - mu0) / se, 2)
  paste(
    "L\u00f6sungsweg: $$\\bar{x}=", "\\frac{", paste(x, collapse = " + "), "}{", n, "}=", xbar, "$$",
    "\n",
    "$$ s=\\sqrt{\\dfrac{\\displaystyle", paste(paste0("(", x, "-", xbar, ")^2"), collapse = " + "), "}{", n, "-1}}=", s, "$$",
    "\n",
    "$$ t_{emp} = \\frac{\\bar{x} - \\mu_0}{\\frac{s}{\\sqrt{n}}} = \\frac{", xbar, " - ", mu0, "}{\\frac{", s, "}{\\sqrt{", n, "}}} = \\frac{", round(xbar - mu0, 2), "}{", se, "} =", t_emp, "$$",
    "\n",
    "$$ df=n-1=", df, "$$",
    "\n",
    "$$ t_{krit(", df, ",", p_crit, ")}=", round(qt(p_crit, df), 3), "$$"
  )
}

latex_indep_ttest_loesungsweg <- function(x_g1, x_g2, two_sided) {
  n1     <- length(x_g1); n2 <- length(x_g2)
  p_crit <- if (two_sided) 0.975 else 0.95
  df     <- n1 + n2 - 2
  paste(
    "L\u00f6sungsweg: $$\\bar{x}_{A}=", "\\frac{", paste(x_g1, collapse = " + "), "}{", n1, "}=", round(mean(x_g1), 2), "$$",
    "\n",
    "$$\\bar{x}_{B}=", "\\frac{", paste(x_g2, collapse = " + "), "}{", n2, "}=", round(mean(x_g2), 2), "$$",
    "\n",
    "$$ \\sigma_{A}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_g1, "-", round(mean(x_g1), 2), ")^2"), collapse = " + "), "}{", n1, "-1}=", round(var(x_g1), 2), "$$",
    "\n",
    "$$ \\sigma_{B}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_g2, "-", round(mean(x_g2), 2), ")^2"), collapse = " + "), "}{", n2, "-1}=", round(var(x_g2), 2), "$$",
    "\n",
    "$$ t_{emp} = \\frac{", round(mean(x_g1), 2), "-", round(mean(x_g2), 2), "}{\\sqrt{\\frac{(", n1, " - 1) \\cdot", round(var(x_g1), 2), "+ (", n2, " - 1) \\cdot", round(var(x_g2), 2), "}{(", n1, " - 1) + (", n2, " - 1)} \\cdot \\left(\\frac{1}{", n1, "} + \\frac{1}{", n2, "} \\right)}} $$",
    "\n",
    "$$ t_{emp} = \\frac{", round(round(mean(x_g1), 2) - round(mean(x_g2), 2), 2), "}{\\sqrt{\\frac{", n1 - 1, " \\cdot", round(var(x_g1), 2), "+", n2 - 1, " \\cdot", round(var(x_g2), 2), "}{", n1 - 1, "+", n2 - 1, "} \\cdot ", round(1/n1 + 1/n2, 2), "}} = \\frac{", round(round(mean(x_g1), 2) - round(mean(x_g2), 2), 2), "}{\\sqrt{\\frac{", (n1 - 1) * round(var(x_g1), 2), "+", (n2 - 1) * round(var(x_g2), 2), "}{", (n1 - 1) + (n2 - 1), "} \\cdot ", round(1/n1 + 1/n2, 2), "}} = \\frac{", round(round(mean(x_g1), 2) - round(mean(x_g2), 2), 2), "}{\\sqrt{", ((n1 - 1) * round(var(x_g1), 2) + (n2 - 1) * round(var(x_g2), 2)) / ((n1 - 1) + (n2 - 1)) * round(1/n1 + 1/n2, 2), "}} = ", round(t.test(x_g1, x_g2)$statistic, 2), " $$",
    "\n",
    "$$ df=n-2=", df, "$$",
    "\n",
    "$$ t_{krit(", df, ",", p_crit, ")}=", round(qt(p_crit, df), 3), "$$"
  )
}

latex_dep_ttest_loesungsweg <- function(diff, df_t, two_sided) {
  p_crit <- if (two_sided) 0.975 else 0.95
  n      <- nrow(df_t)
  paste(
    "L\u00f6sungsweg: $$\\bar{x}_d = \\frac{\\sum\\limits _{i=1}^{N}d_{i}}{N}$$",
    "\n",
    "$$\\bar{x}_d = \\frac{", paste(diff, collapse = " + "), "}{", n, "}$$",
    "\n",
    "$$\\bar{x}_d = \\frac{", sum(diff), "}{", n, "}$$",
    "\n",
    "$$\\bar{x}_d =", round(sum(diff) / n, 2), "$$",
    "\n",
    "$$\\hat{\\sigma}_{\\bar{x}_d} = \\frac{\\sqrt{\\frac{\\sum\\limits _{i=1}^{N}(d_{i}-\\bar{x}_d)^2}{N-1}}}{\\sqrt{N}}$$",
    "\n",
    "$$\\hat{\\sigma}_{\\bar{x}_d} = \\frac{\\sqrt{\\frac{", paste(paste0("(", diff, "-", round(mean(diff), 2), ")^2"), collapse = " + "), "}{", n, "-1}}}{\\sqrt{", n, "}}$$",
    "\n",
    "$$\\hat{\\sigma}_{\\bar{x}_d} = \\frac{\\sqrt{", round(var(diff), 2), "}}{\\sqrt{", n, "}} = \\frac{", round(sqrt(var(diff)), 2), "}{", round(sqrt(n), 2), "}=", round(round(sqrt(var(diff)), 2) / round(sqrt(n), 2), 2), "$$",
    "\n",
    "$$t_{emp}=\\frac{", round(mean(diff), 2), "}{", round(round(sqrt(var(diff)), 2) / round(sqrt(n), 2), 2), "}=", round(round(mean(diff), 2) / round(round(sqrt(var(diff)), 2) / round(sqrt(n), 2), 2), 2), "$$",
    "\n",
    "$$ df=n-1=", n - 1, "$$",
    "\n",
    "$$ t_{krit(", n - 1, ",", p_crit, ")}=", round(qt(p_crit, n - 1), 3), "$$"
  )
}

# UI -----------------------------------------------------------------------

task_choices <- setNames(aufgabe$task, aufgabe$label)

ui <- fluidPage(
  tags$img(src = "logocfh.png", style = "width: 360px; height: auto;"),
  headerPanel("Statistik Trainer"),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "drop",
        label    = "Was m\u00f6chten Sie trainieren?",
        choices  = task_choices,
        multiple = FALSE,
        selectize = TRUE
      ),
      actionButton("go", "Go"),
      uiOutput("table_button"),
      uiOutput("solve_button"),
      uiOutput("reset_button"),
      width = 4
    ),
    mainPanel(
      verticalLayout(
        uiOutput("text"),
        br(),
        tableOutput("table"),
        tableOutput("table_alt"),
        uiOutput("text2"),
        tableOutput("table2"),
        tableOutput("table3"),
        uiOutput("formel"),
        uiOutput("loesungsweg"),
        uiOutput("loesung"),
        uiOutput("loesung_table_label"),
        tableOutput("loesung_table"),
        uiOutput("plot_label"),
        uiOutput("plot_container")
      ),
      width = 8
    )
  )
)

# Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Generate all data when "Go" is pressed ---------------------------------

  sim_data <- eventReactive(input$go, {
    n         <- sample(ns_vals, 1)
    mean_val  <- sample(means_vals, 1)
    mean_val2 <- sample(means_vals, 1)
    sd1       <- mean_val * sample(sds_vals, 1)
    sd2       <- mean_val2 * sample(sds_vals, 1)
    r         <- sample(cor_vals, 1)

    df_vanilla <- round(data.frame(mvrnorm(
      n = n, mu = c(mean_val, mean_val2),
      Sigma = matrix(c(sd1, r, r, sd2), nrow = 2),
      empirical = FALSE
    )))
    names(df_vanilla) <- c("X", "Y")
    x <- df_vanilla$X
    y <- df_vanilla$Y

    df        <- as.data.frame(t(data.frame(ID = as.character(1:n), X = x)))
    df_2      <- as.data.frame(t(data.frame(ID = as.character(1:n), X = x, Y = y)))
    df_2_plot <- data.frame(ID = as.character(1:n), X = x, Y = y)

    x_rank   <- sample(1:n, size = n, replace = FALSE)
    y_rank   <- sample(1:n, size = n, replace = FALSE)
    df_2rank <- as.data.frame(t(data.frame(ID = as.character(1:n), X = x_rank, Y = y_rank)))

    # 2 groups
    d_val <- sample(cohend_vals, 1)
    x_g1  <- round(rnorm(n / 2, mean_val, sd1))
    x_g2  <- round(rnorm(n / 2, d_val * sd1 + mean_val, sd1))
    if (mean(x_g2) > mean(x_g1)) {
      tmp <- x_g1; x_g1 <- x_g2; x_g2 <- tmp
    }
    df_gr <- as.data.frame(t(rbind(
      data.frame(ID = as.character(1:(n/2)), X = x_g1, Gruppe = "A"),
      data.frame(ID = as.character(1:(n/2)), X = x_g2, Gruppe = "B")
    )))
    df_t <- data.frame(ID = 1:(n/2), t0 = x_g1, t1 = x_g2)
    diff <- df_t$t0 - df_t$t1

    # categorical 2 groups (xtable)
    proportion  <- seq(0.1, 0.9, 0.1)
    x_g1_d <- rep("X1", n/2)
    x_g1_d[sample(n/2, round(n/2 * sample(proportion, 1)))] <- "X2"
    x_g2_d <- rep("X1", n/2)
    x_g2_d[sample(n/2, round(n/2 * sample(proportion, 1)))] <- "X2"
    df_gr_d_vanilla <- rbind(
      data.frame(ID = as.character(1:(n/2)), X = x_g1_d, Gruppe = "A"),
      data.frame(ID = as.character(1:(n/2)), X = x_g2_d, Gruppe = "B")
    )
    df_gr_d <- as.data.frame(t(df_gr_d_vanilla))

    # 3 groups — use n3 = 12 so groups are always equal integers
    n3    <- 12
    d1_3g <- sample(cohend_vals, 1)
    d2_3g <- sample(cohend_vals, 1)
    x_3g1 <- round(rnorm(n3/3, mean_val, sd1))
    x_3g2 <- round(rnorm(n3/3, d1_3g * sd1 + mean_val, sd1))
    x_3g3 <- round(rnorm(n3/3, d2_3g * sd1 + mean_val, sd1))
    df_3gr <- as.data.frame(t(rbind(
      data.frame(ID = as.character(1:(n3/3)), X = x_3g1, Gruppe = "A"),
      data.frame(ID = as.character(1:(n3/3)), X = x_3g2, Gruppe = "B"),
      data.frame(ID = as.character(1:(n3/3)), X = x_3g3, Gruppe = "C")
    )))

    # Repeated measures ANOVA (3 time points, 6 subjects)
    n_rm    <- 6
    base_rm <- round(rnorm(n_rm, mean_val, sd1))
    eff_t1  <- sample(seq(-sd1, sd1, length.out = 9), 1)
    eff_t2  <- sample(seq(-sd1, sd1, length.out = 9), 1)
    noise   <- sd1 * 0.3
    rm_t0   <- round(base_rm + rnorm(n_rm, 0, noise))
    rm_t1   <- round(base_rm + eff_t1 + rnorm(n_rm, 0, noise))
    rm_t2   <- round(base_rm + eff_t2 + rnorm(n_rm, 0, noise))
    df_rm   <- data.frame(ID = 1:n_rm, t0 = rm_t0, t1 = rm_t1, t2 = rm_t2)
    df_rm_t <- as.data.frame(t(df_rm))

    zdist_pos       <- sample(1:n, 1)
    zdist_direction <- sample(c("lower", "upper"), 1)

    n_binom <- sample(5:10, 1)
    p_binom <- sample(seq(0.1, 0.9, 0.1), 1)
    k_binom <- sample(0:n_binom, 1)

    p_bernoulli <- sample(seq(0.1, 0.9, 0.1), 1)
    k_bernoulli <- sample(0:1, 1)

    k_alpha   <- sample(2:10, 1)
    k_fdr     <- sample(2:10, 1)

    # eta2 / omega2: random ANOVA result
    eff_df1   <- sample(1:4, 1)
    eff_df2   <- sample(10:60, 1)
    eff_N     <- eff_df1 + eff_df2 + 1
    eff_F     <- round(runif(1, 1.5, 12), 2)
    p_fdr_raw <- sort(round(runif(k_fdr, 0.001, 0.2), 3))

    # Chi-square tests
    n_chi2 <- sample(c(20, 25, 30, 40, 50), 1)

    # Gleichverteilung
    k_chi2_gleich  <- sample(3:5, 1)
    obs_chi2_gleich <- as.vector(rmultinom(1, n_chi2, rep(1/k_chi2_gleich, k_chi2_gleich)))

    # Verh\u00e4ltnis
    ratios_options <- list(c(0.2, 0.8), c(0.3, 0.7), c(0.4, 0.6), c(0.25, 0.75),
                           c(1/3, 2/3), c(0.2, 0.3, 0.5), c(0.25, 0.25, 0.5))
    probs_verh   <- ratios_options[[sample(length(ratios_options), 1)]]
    obs_chi2_verh <- as.vector(rmultinom(1, n_chi2, probs_verh))

    # 2D (2 Zeilen x 2-4 Spalten)
    cols_2d      <- sample(2:4, 1)
    obs_chi2_2d  <- matrix(sample(5:20, 2 * cols_2d, replace = TRUE), nrow = 2,
                           dimnames = list(c("A", "B"), paste0("K", 1:cols_2d)))

    # 4-Felder (2x2)
    obs_chi2_4feld <- matrix(sample(5:25, 4, replace = TRUE), nrow = 2,
                             dimnames = list(c("A", "B"), c("K1", "K2")))

    # Phi: generate plausible chi² from a target phi (2x2 table)
    N_phi      <- sample(c(30, 40, 50, 60, 80, 100), 1)
    phi_target <- round(runif(1, 0.05, 0.75), 2)
    chi2_phi   <- round(phi_target^2 * N_phi, 2)

    # Cramér's V: generate plausible chi² from a target V
    k_cramersv    <- sample(2:4, 1)
    N_cramersv    <- sample(c(30, 40, 50, 60, 80, 100), 1)
    V_target      <- round(runif(1, 0.05, 0.75), 2)
    chi2_cramersv <- round(V_target^2 * N_cramersv * (k_cramersv - 1), 2)

    # Ein-Stichproben t-Test Referenzwert (nah am Stichprobenmittelwert)
    mu0    <- round(mean(x)) + sample(-3:3, 1)

    # Vorhersage
    x_pred <- round(runif(1, min(x), max(x)))

    list(
      n = n, n3 = n3,
      zdist_pos = zdist_pos, zdist_direction = zdist_direction,
      n_binom = n_binom, p_binom = p_binom, k_binom = k_binom,
      p_bernoulli = p_bernoulli, k_bernoulli = k_bernoulli,
      k_alpha = k_alpha, k_fdr = k_fdr, p_fdr_raw = p_fdr_raw,
      eff_df1 = eff_df1, eff_df2 = eff_df2, eff_N = eff_N, eff_F = eff_F,
      x = x, y = y,
      df = df, df_2 = df_2, df_2_plot = df_2_plot,
      x_rank = x_rank, y_rank = y_rank, df_2rank = df_2rank,
      x_g1 = x_g1, x_g2 = x_g2, df_gr = df_gr, df_t = df_t, diff = diff,
      df_gr_d = df_gr_d, df_gr_d_vanilla = df_gr_d_vanilla,
      x_3g1 = x_3g1, x_3g2 = x_3g2, x_3g3 = x_3g3, df_3gr = df_3gr,
      n_chi2 = n_chi2, k_chi2_gleich = k_chi2_gleich,
      obs_chi2_gleich = obs_chi2_gleich, probs_verh = probs_verh,
      obs_chi2_verh = obs_chi2_verh, obs_chi2_2d = obs_chi2_2d,
      obs_chi2_4feld = obs_chi2_4feld,
      N_phi = N_phi, chi2_phi = chi2_phi,
      k_cramersv = k_cramersv, N_cramersv = N_cramersv, chi2_cramersv = chi2_cramersv,
      df_rm = df_rm, df_rm_t = df_rm_t, n_rm = n_rm,
      mu0 = mu0, x_pred = x_pred
    )
  })

  # Reactives ---------------------------------------------------------------

  ordinals_de <- c("erste", "zweite", "dritte", "vierte", "f\u00fcnfte", "sechste",
                   "siebte", "achte", "neunte", "zehnte", "elfte", "zw\u00f6lfte")

  text_reactive <- eventReactive(input$go, {
    text <- if (input$drop == "binom") {
      d <- sim_data()
      paste0("Aufgabe: Eine Zufallsvariable \\(X\\) ist binomialverteilt mit \\(n = ", d$n_binom,
             "\\) Versuchen und einer Erfolgswahrscheinlichkeit von \\(p = ", d$p_binom,
             "\\). Berechnen Sie \\(P(X = ", d$k_binom, ")\\).")
    } else if (input$drop == "bernoulli") {
      d <- sim_data()
      paste0("Aufgabe: Eine Zufallsvariable \\(X\\) ist bernoulliverteilt mit einer Erfolgswahrscheinlichkeit von \\(p = ", d$p_bernoulli,
             "\\). Berechnen Sie \\(P(X = ", d$k_bernoulli, ")\\). ",
             "(Hinweis: Die Bernoulliverteilung ist ein Spezialfall der Binomialverteilung mit \\(n = 1\\).)")
    } else if (input$drop %in% c("t.test1", "t.test1_2")) {
      d <- sim_data()
      side_text <- if (input$drop == "t.test1") "zweiseitigen" else "einseitigen"
      paste0("Aufgabe: Berechnen Sie einen ", side_text, " Ein-Stichproben t-Test f\u00fcr die Variable \\(X\\) ",
             "gegen den Referenzwert \\(\\mu_0 = ", d$mu0, "\\) (\\(\\alpha = .05\\)).")
    } else if (input$drop == "alpha_kumulierung") {
      d <- sim_data()
      paste0("Aufgabe: Ein Forscher f\u00fchrt statt einem Test insgesamt \\(k = ", d$k_alpha,
             "\\) Tests durch, jeweils zum Signifikanzniveau \\(\\alpha = 0.05\\). ",
             "Berechnen Sie die Wahrscheinlichkeit, mindestens einen Fehler 1. Art zu begehen.")
    } else if (input$drop == "bonferroni") {
      d <- sim_data()
      paste0("Aufgabe: Ein Forscher f\u00fchrt insgesamt \\(k = ", d$k_alpha,
             "\\) Tests durch. Berechnen Sie das Bonferroni-korrigierte Signifikanzniveau \\(\\alpha^*\\) f\u00fcr \\(\\alpha = 0.05\\).")
    } else if (input$drop == "fdr") {
      d <- sim_data()
      p_str <- paste(d$p_fdr_raw, collapse = ", ")
      paste0("Aufgabe: Bei \\(k = ", d$k_fdr, "\\) Tests wurden die folgenden p-Werte beobachtet: \\(p = (", p_str, ")\\). ",
             "Wenden Sie die Benjamini-Hochberg-Korrektur an (\\(\\alpha = 0.05\\)) und bestimmen Sie, welche Tests signifikant sind.")
    } else if (input$drop == "eta2") {
      d <- sim_data()
      paste0("Aufgabe: Sie haben eine Varianzanalyse berechnet und erhalten folgendes Ergebnis: ",
             "\\(F(", d$eff_df1, ", ", d$eff_df2, ") = ", d$eff_F, "\\), \\(N = ", d$eff_N, "\\). ",
             "Berechnen Sie \\(\\eta^2\\) als Effektst\u00e4rkema\u00df.")
    } else if (input$drop == "omega2") {
      d <- sim_data()
      paste0("Aufgabe: Sie haben eine Varianzanalyse berechnet und erhalten folgendes Ergebnis: ",
             "\\(F(", d$eff_df1, ", ", d$eff_df2, ") = ", d$eff_F, "\\), \\(N = ", d$eff_N, "\\). ",
             "Berechnen Sie \\(\\omega^2\\) als Effektst\u00e4rkema\u00df.")
    } else if (input$drop == "phi") {
      d <- sim_data()
      paste0("Aufgabe: Sie haben einen 2\u00d72 Chi-Quadrat-Test berechnet und erhalten folgendes Ergebnis: ",
             "\\(\\chi^2 = ", d$chi2_phi, "\\), \\(N = ", d$N_phi, "\\). ",
             "Berechnen Sie \\(\\phi\\) als Effektst\u00e4rkema\u00df.")
    } else if (input$drop == "cramersv") {
      d <- sim_data()
      paste0("Aufgabe: Sie haben einen Chi-Quadrat-Test berechnet und erhalten folgendes Ergebnis: ",
             "\\(\\chi^2 = ", d$chi2_cramersv, "\\), \\(N = ", d$N_cramersv, "\\), \\(k = ", d$k_cramersv, "\\) ",
             "(kleinere Dimension der Kontingenztabelle). ",
             "Berechnen Sie Cram\u00e9rs \\(V\\) als Effektst\u00e4rkema\u00df.")
    } else if (input$drop == "chi2_verh") {
      d <- sim_data()
      pct <- round(d$probs_verh * 100)
      pct_str <- paste(paste0(pct, "%"), collapse = " / ")
      paste0("Aufgabe: Testen Sie, ob die H\u00e4ufigkeitsverteilung der Variable X dem Verh\u00e4ltnis ",
             pct_str, " entspricht (\\(\\alpha = .05\\)).")
    } else if (input$drop == "zdist") {
      d <- sim_data()
      ord       <- ordinals_de[d$zdist_pos]
      direction <- if (d$zdist_direction == "lower") "kleineren" else "gr\u00f6\u00dferen"
      paste0("Aufgabe: Bestimmen Sie f\u00fcr die ", ord, " Auspr\u00e4gung der Variable \\(X\\) ",
             "die Wahrscheinlichkeit, diesen oder einen ", direction, " Wert zu erhalten. ",
             "Die Wahrscheinlichkeit kann aus der z-Tabelle abgelesen werden:")
    } else {
      aufgabe$ask[aufgabe$task == input$drop]
    }
    sub("^Aufgabe:", "<strong>Aufgabe:</strong>", text)
  })

  text2_reactive <- eventReactive(input$go, {
    d <- sim_data()
    switch(input$drop,
      "t.test1" = {
        paste0("Hypothesen: $$H_0 : \\mu = ", d$mu0, "$$\n$$H_1 : \\mu \\neq ", d$mu0, "$$")
      },
      "t.test1_2" = {
        paste0("Hypothesen: $$H_0 : \\mu \\leq ", d$mu0, "$$\n$$H_1 : \\mu > ", d$mu0, "$$")
      },
      "t.test"       = paste("Hypothesen: $$H_0", ":", " \\mu_A - \\mu_B = 0$$", "\n", "$$H_1", ":", " \\mu_A - \\mu_B \\neq 0$$"),
      "t.test2"      = paste("Hypothesen: $$H_0", ":", " \\mu_A - \\mu_B \\leq 0$$", "\n", "$$H_1", ":", " \\mu_A - \\mu_B > 0$$"),
      "abh_t.test"   = paste("Hypothesen: $$H_0", ":", " \\mu_d = 0$$", "\n", "$$H_1", ":", " \\mu_d \\neq 0$$"),
      "abh_t.test2"  = paste("Hypothesen: $$H_0", ":", " \\mu_d \\leq 0$$", "\n", "$$H_1", ":", " \\mu_d > 0$$"),
      "oneway_anova" = paste("Hypothesen: $$H_0", ":", " \\mu_1 = \\mu_2 = \\mu_3$$", "\n", "$$H_1", ":", " \\exists \\ i, j \\ \\text{mit} \\ \\mu_i \\neq \\mu_j$$"),
      "rm_anova"     = paste("Hypothesen: $$H_0 : \\mu_{t_0} = \\mu_{t_1} = \\mu_{t_2}$$", "\n", "$$H_1 : \\exists \\ i, j \\ \\text{mit} \\ \\mu_i \\neq \\mu_j$$"),
      "chi2_gleich" = paste("Hypothesen: $$H_0 : \\text{Gleichverteilung}$$",
                            "\n", "$$H_1 : \\text{keine Gleichverteilung}$$"),
      "chi2_verh"   = {
        d <- sim_data()
        pct <- round(d$probs_verh * 100)
        pct_str <- paste(paste0(pct, "%"), collapse = " / ")
        paste0("Hypothesen: $$H_0 : \\text{Verteilung entspricht Verh\u00e4ltnis } (", pct_str, ")$$\n",
               "$$H_1 : \\text{Verteilung entspricht nicht dem Verh\u00e4ltnis}$$")
      },
      "chi2_2d" = paste("Hypothesen: $$H_0 : \\text{X und Y sind unabh\u00e4ngig}$$",
                        "\n", "$$H_1 : \\text{X und Y sind abh\u00e4ngig}$$"),
      "chi2_4feld" = paste("Hypothesen: $$H_0 : \\text{X und Y sind unabh\u00e4ngig}$$",
                           "\n", "$$H_1 : \\text{X und Y sind abh\u00e4ngig}$$"),
      "confint_mean_with_var" = paste("Gegeben: $$\\bar{x}=", round(mean(d$x), 2), "$$", "\n", "$$ \\sigma^2=", round(var(d$x), 2), "$$", "\n", "$$ \\alpha = .05 $$"),
      "confint_mean_no_var"   = paste("Gegeben: $$\\bar{x}=", round(mean(d$x), 2), "$$", "\n", "$$ s^2=", round(var(d$x), 2), "$$", "\n", "$$ \\alpha = .05 $$"),
      "lm_predict" = {
        a_val <- round(mean(d$y) - (cov(d$x, d$y) / var(d$x)) * mean(d$x), 2)
        b_val <- round(cov(d$x, d$y) / var(d$x), 2)
        paste0("Modell: $$ \\hat{y} = ", a_val, " + ", b_val, " \\cdot x $$",
               "\n", "Vorherzusagender Wert: $$ x = ", d$x_pred, " $$")
      },
      "lm_std" = {
        a_val <- round(mean(d$y) - (cov(d$x, d$y) / var(d$x)) * mean(d$x), 2)
        b_val <- round(cov(d$x, d$y) / var(d$x), 2)
        paste0("Modell: $$ \\hat{y} = ", a_val, " + ", b_val, " \\cdot x $$")
      },
      "lm_sig" = {
        a_val <- round(mean(d$y) - (cov(d$x, d$y) / var(d$x)) * mean(d$x), 2)
        b_val <- round(cov(d$x, d$y) / var(d$x), 2)
        paste0("Modell: $$ \\hat{y} = ", a_val, " + ", b_val, " \\cdot x $$\n",
               "Hypothesen: $$H_0 : b = 0$$\n$$H_1 : b \\neq 0$$")
      },
      "lm_sig2" = {
        a_val <- round(mean(d$y) - (cov(d$x, d$y) / var(d$x)) * mean(d$x), 2)
        b_val <- round(cov(d$x, d$y) / var(d$x), 2)
        paste0("Modell: $$ \\hat{y} = ", a_val, " + ", b_val, " \\cdot x $$\n",
               "Hypothesen: $$H_0 : b \\leq 0$$\n$$H_1 : b > 0$$")
      },
      "cor_sig" = {
        r_val <- round(cor(d$x, d$y), 2)
        paste0("Gegeben: $$ r = ", r_val, ", \\quad N = ", d$n, " $$\n",
               "Hypothesen: $$H_0 : \\rho = 0$$\n$$H_1 : \\rho \\neq 0$$")
      },
      "cor_sig2" = {
        r_val <- round(cor(d$x, d$y), 2)
        paste0("Gegeben: $$ r = ", r_val, ", \\quad N = ", d$n, " $$\n",
               "Hypothesen: $$H_0 : \\rho \\leq 0$$\n$$H_1 : \\rho > 0$$")
      },
      "cor_sig_s" = {
        x_rank <- d$x_rank; y_rank <- d$y_rank; n_val <- d$n
        rs_val <- round(1 - 6 * sum((x_rank - y_rank)^2) / (n_val * (n_val^2 - 1)), 2)
        paste0("Gegeben: $$ r_s = ", rs_val, ", \\quad N = ", d$n, " $$\n",
               "Hypothesen: $$H_0 : \\rho_s = 0$$\n$$H_1 : \\rho_s \\neq 0$$")
      },
      "cor_sig_s2" = {
        x_rank <- d$x_rank; y_rank <- d$y_rank; n_val <- d$n
        rs_val <- round(1 - 6 * sum((x_rank - y_rank)^2) / (n_val * (n_val^2 - 1)), 2)
        paste0("Gegeben: $$ r_s = ", rs_val, ", \\quad N = ", d$n, " $$\n",
               "Hypothesen: $$H_0 : \\rho_s \\leq 0$$\n$$H_1 : \\rho_s > 0$$")
      },
      NULL
    )
  })

  table_reactive <- eventReactive(input$go, {
    d <- sim_data()
    if (input$drop == "xtable")                                              return(d$df_gr_d)
    if (input$drop %in% c("cov", "cor_pearson", "cor_sig", "cor_sig2", "lm", "lm_predict", "lm_std", "lm_sig", "lm_sig2")) return(d$df_2)
    if (input$drop %in% c("cor_spearman", "cor_sig_s", "cor_sig_s2"))       return(d$df_2rank)
    if (input$drop %in% c("t.test", "t.test2", "cohend"))                   return(d$df_gr)
    if (input$drop == "oneway_anova")                                        return(d$df_3gr)
    if (input$drop == "rm_anova")                                           return(d$df_rm_t)
    if (input$drop %in% c("modalwert", "mean", "median", "spannweite", "sd", "var", "iqr", "boxplot", "scale",
                           "zdist", "confint_mean_with_var",
                           "confint_mean_no_var", "t.test1", "t.test1_2"))  return(d$df)
    if (input$drop == "fdr") {
      return(as.data.frame(t(data.frame(
        Test = paste0("T", seq_along(d$p_fdr_raw)),
        p    = d$p_fdr_raw
      ))))
    }
  })

  table_alt_reactive <- eventReactive(input$go, {
    d <- sim_data()
    if (input$drop %in% c("abh_t.test", "abh_t.test2")) {
      m <- d$df_t
      names(m) <- c("ID", "t<sub>0</sub>", "t<sub>1</sub>")
      return(m)
    }
    if (input$drop == "chi2_gleich") {
      k <- d$k_chi2_gleich; obs <- d$obs_chi2_gleich; n <- sum(obs)
      return(data.frame(Kategorie = paste0("K", 1:k), Beobachtet = obs,
                        Erwartet = rep(round(n / k, 1), k)))
    }
    if (input$drop == "chi2_verh") {
      obs <- d$obs_chi2_verh; probs <- d$probs_verh; n <- sum(obs); k <- length(obs)
      return(data.frame(Kategorie = paste0("K", 1:k), Beobachtet = obs,
                        Anteil = paste0(round(probs * 100), "%")))
    }
    if (input$drop %in% c("chi2_2d", "chi2_4feld")) {
      mat <- if (input$drop == "chi2_2d") d$obs_chi2_2d else d$obs_chi2_4feld
      df_out <- as.data.frame(mat)
      df_out <- cbind(Gruppe = rownames(mat), df_out)
      return(df_out)
    }
  })

  table2_reactive <- eventReactive(input$tab, {
    if (input$drop == "zdist") {
      z0  <- seq(0, 3, 0.1)
      z00 <- seq(0, 0.09, 0.01)
      m   <- outer(z0, z00, FUN = function(a, b) pnorm(a + b))
      m   <- cbind(z0, m)
      colnames(m) <- c("z", format(z00, decimal.mark = ","))
      m[, 1] <- round(m[, 1], 2)
      m[, 2:ncol(m)] <- round(m[, 2:ncol(m)], 4)
      m <- as.data.frame(m)
      m[, 1] <- as.character(m[, 1])
      return(m)
    }
    if (input$drop %in% c("t.test1", "t.test1_2", "abh_t.test", "abh_t.test2", "t.test", "t.test2", "lm_sig", "lm_sig2", "cor_sig", "cor_sig2", "cor_sig_s", "cor_sig_s2")) {
      t_quantiles <- c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90,
                       0.95, 0.975, 0.990, 0.995, 0.9995)
      df_tbl <- data.frame(df = c(1:30, 40, 60, 120))
      for (p in t_quantiles) {
        df_tbl[, as.character(p)] <- as.character(abs(round(qt(1 - p, df_tbl$df), 3)))
      }
      df_tbl$df <- as.character(df_tbl$df)
      return(df_tbl)
    }
    if (input$drop %in% c("chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld")) {
      chi2_quants <- c(0.75, 0.90, 0.95, 0.975, 0.99)
      df_tbl <- data.frame(df = as.character(1:20))
      for (p in chi2_quants) {
        df_tbl[, as.character(p)] <- round(qchisq(p, 1:20), 3)
      }
      return(df_tbl)
    }
    if (input$drop %in% c("oneway_anova", "rm_anova")) {
      f_quantiles <- c(0.75, 0.90, 0.95, 0.99)
      flaeche_col <- "Fl\u00e4che"
      d_tbl <- data.frame(df2 = rep(1:12, each = 4), Flaeche = rep(f_quantiles, 12))
      names(d_tbl)[2] <- flaeche_col
      d_tbl <- d_tbl[-4, ]
      for (df1 in 1:12) {
        d_tbl[, as.character(df1)] <- NA
        for (df2 in 1:12) {
          for (p in f_quantiles) {
            d_tbl[d_tbl$df2 == df2 & d_tbl[[flaeche_col]] == p, as.character(df1)] <-
              round(qf(p = p, df1 = df1, df2 = df2), 2)
          }
        }
      }
      names(d_tbl)[1] <- "df2 \\ df1"
      d_tbl[[flaeche_col]] <- format(d_tbl[[flaeche_col]], nsmall = 2)
      return(d_tbl)
    }
  })

  table3_reactive <- eventReactive(input$solve, {
    d <- sim_data()
    if (input$drop %in% c("abh_t.test", "abh_t.test2")) {
      m <- d$df_t
      m$d <- m$t0 - m$t1
      names(m) <- c("ID", "t<sub>0</sub>", "t<sub>1</sub>", "d (t<sub>0</sub> - t<sub>1</sub>)")
      return(m)
    }
  })

  formel_reactive <- eventReactive(input$solve, {
    aufgabe$formel[aufgabe$task == input$drop]
  })

  loesung_table_reactive <- eventReactive(input$solve, {
    d <- sim_data()
    if (input$drop == "xtable") {
      tbl <- as.data.frame.matrix(addmargins(table(d$df_gr_d_vanilla[, 2:3])))
      rownames(tbl)[rownames(tbl) == "Sum"] <- "\u03a3"
      colnames(tbl)[colnames(tbl) == "Sum"] <- "\u03a3"
      return(tbl)
    }
  })

  loesung_reactive <- eventReactive(input$solve, {
    d    <- sim_data()
    x    <- d$x; y <- d$y; n <- d$n
    x_g1 <- d$x_g1; x_g2 <- d$x_g2
    x_rank <- d$x_rank; y_rank <- d$y_rank
    df_t <- d$df_t; diff <- d$diff

    switch(input$drop,
      "modalwert" = {
        x_mod <- as.numeric(names(which.max(table(x))))
        paste("L\u00f6sung: $$ x_{mod} =", x_mod, "$$")
      },
      "mean"    = paste("L\u00f6sung: $$\\bar{x}=", round(mean(x), 2), "$$"),
      "median"  = paste("L\u00f6sung: $$\\textit{Md}=", round(median(x), 2), "$$"),
      "spannweite" = paste("L\u00f6sung: $$ R =", max(x), "-", min(x), "=", max(x) - min(x), "$$"),
      "var"     = paste("L\u00f6sung: $$ s^2=", round(var(x), 2), "$$"),
      "sd"      = paste("L\u00f6sung: $$ s=", round(sd(x), 2), "$$"),
      "iqr" = {
        xs <- sort(x)
        if ((n * 0.25) %% 1 == 0) {
          q1 <- (xs[n * 0.25] + xs[n * 0.25 + 1]) / 2
          q3 <- (xs[n * 0.75] + xs[n * 0.75 + 1]) / 2
        } else {
          q1 <- xs[ceiling(n * 0.25)]
          q3 <- xs[ceiling(n * 0.75)]
        }
        paste("L\u00f6sung: $$ IQR =", q3 - q1, "$$")
      },
      "boxplot" = {
        xs <- sort(x)
        md <- if (n %% 2 == 0) (xs[n/2] + xs[n/2+1]) / 2 else xs[(n+1)/2]
        if ((n * 0.25) %% 1 == 0) {
          q1 <- (xs[n * 0.25] + xs[n * 0.25 + 1]) / 2
          q3 <- (xs[n * 0.75] + xs[n * 0.75 + 1]) / 2
        } else {
          q1 <- xs[ceiling(n * 0.25)]
          q3 <- xs[ceiling(n * 0.75)]
        }
        iqr_val  <- q3 - q1
        w_low    <- min(xs[xs >= q1 - 1.5 * iqr_val])
        w_high   <- max(xs[xs <= q3 + 1.5 * iqr_val])
        outliers <- xs[xs < q1 - 1.5 * iqr_val | xs > q3 + 1.5 * iqr_val]
        out_str  <- if (length(outliers) > 0) paste0(", \\text{ Ausrei\u00dfer: } ", paste(outliers, collapse = ", ")) else ""
        paste0("L\u00f6sung: $$ \\tilde{x}_{0.50} = ", md,
               ", \\quad \\tilde{x}_{0.25} = ", q1,
               ", \\quad \\tilde{x}_{0.75} = ", q3, " $$",
               "$$ IQR = ", iqr_val,
               ", \\quad W_{unten} = ", w_low,
               ", \\quad W_{oben} = ", w_high, out_str, " $$")
      },
      "scale"        = paste("L\u00f6sung: $$ z_i=", paste(as.character(round(scale(x), 2)), collapse = "; "), "$$"),
      "zdist" = {
        pos   <- d$zdist_pos
        z_pos <- round(scale(x), 2)[pos]
        p_val <- if (d$zdist_direction == "lower") round(pnorm(z_pos), 4) else round(1 - pnorm(z_pos), 4)
        p_sym <- if (d$zdist_direction == "lower") paste0("P(X \\leq x_{", pos, "})") else paste0("P(X > x_{", pos, "})")
        paste0("L\u00f6sung: $$ z_{", pos, "}=", z_pos, " $$ $$ ", p_sym, "=", p_val, " $$")
      },
      "cov"          = paste("L\u00f6sung: $$ s_{xy}=", round(cov(x, y), 2), "$$"),
      "binom" = {
        nb <- d$n_binom; kb <- d$k_binom; pb <- d$p_binom
        paste0("L\u00f6sung: $$ P(X = ", kb, ") = ", round(dbinom(kb, nb, pb), 4), " $$")
      },
      "bernoulli" = {
        pb <- d$p_bernoulli; kb <- d$k_bernoulli
        paste0("L\u00f6sung: $$ P(X = ", kb, ") = ", round(pb^kb * (1 - pb)^(1 - kb), 4), " $$")
      },
      "cor_pearson" = {
        r_val <- round(cor(x, y), 2)
        paste0("L\u00f6sung: $$ r=", r_val, " $$", interpret_cor(r_val))
      },
      "cor_spearman" = {
        r_val <- 1 - round(6 * sum((x_rank - y_rank)^2) / (n * (n^2 - 1)), 2)
        paste0("L\u00f6sung: $$ r_s=", r_val, " $$", interpret_cor(r_val))
      },
      "cor_sig" = {
        r_val  <- round(cor(x, y), 2)
        df_val <- n - 2
        t_emp  <- round(r_val * sqrt(df_val) / sqrt(1 - r_val^2), 2)
        t_krit <- round(qt(0.975, df_val), 3)
        decision <- if (abs(t_emp) > t_krit)
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "cor_sig2" = {
        r_val  <- round(cor(x, y), 2)
        df_val <- n - 2
        t_emp  <- round(r_val * sqrt(df_val) / sqrt(1 - r_val^2), 2)
        t_krit <- round(qt(0.95, df_val), 3)
        decision <- if (t_emp > t_krit)
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "cor_sig_s" = {
        rs_val <- round(1 - 6 * sum((x_rank - y_rank)^2) / (n * (n^2 - 1)), 2)
        df_val <- n - 2
        t_emp  <- round(rs_val * sqrt(df_val) / sqrt(1 - rs_val^2), 2)
        t_krit <- round(qt(0.975, df_val), 3)
        decision <- if (abs(t_emp) > t_krit)
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "cor_sig_s2" = {
        rs_val <- round(1 - 6 * sum((x_rank - y_rank)^2) / (n * (n^2 - 1)), 2)
        df_val <- n - 2
        t_emp  <- round(rs_val * sqrt(df_val) / sqrt(1 - rs_val^2), 2)
        t_krit <- round(qt(0.95, df_val), 3)
        decision <- if (t_emp > t_krit)
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "alpha_kumulierung" = {
        k <- d$k_alpha
        result <- round(1 - 0.95^k, 4)
        paste0("L\u00f6sung: $$ P(\\text{mind. 1 Fehler 1. Art}) = ", result, " $$")
      },
      "bonferroni" = {
        k <- d$k_alpha
        result <- round(0.05 / k, 4)
        paste0("L\u00f6sung: $$ \\alpha^* = ", result, " $$")
      },
      "fdr" = {
        p   <- d$p_fdr_raw; k <- d$k_fdr
        p_adj <- round(p * k / seq_along(p), 4)
        sig   <- p_adj <= 0.05
        sig_tests <- if (any(sig)) paste(paste0("T", which(sig)), collapse = ", ") else "keiner"
        paste0("L\u00f6sung: Signifikante Tests (nach BH-Korrektur): ", sig_tests)
      },
      "eta2" = {
        f <- d$eff_F; df1 <- d$eff_df1; df2 <- d$eff_df2
        eta2 <- round(df1 * f / (df1 * f + df2), 3)
        paste0("L\u00f6sung: $$ \\eta^2 = ", eta2, " $$", interpret_eta2(eta2))
      },
      "omega2" = {
        f <- d$eff_F; df1 <- d$eff_df1; df2 <- d$eff_df2; N <- d$eff_N
        omega2 <- round(df1 * (f - 1) / (df1 * (f - 1) + N), 3)
        paste0("L\u00f6sung: $$ \\omega^2 = ", omega2, " $$", interpret_omega2(omega2))
      },
      "cramersv" = {
        chi2 <- d$chi2_cramersv; N <- d$N_cramersv; k <- d$k_cramersv
        V <- round(sqrt(chi2 / (N * (k - 1))), 3)
        paste0("L\u00f6sung: $$ V = ", V, " $$", interpret_cramersv(V))
      },
      "phi" = {
        chi2 <- d$chi2_phi; N <- d$N_phi
        phi  <- round(sqrt(chi2 / N), 3)
        paste0("L\u00f6sung: $$ \\phi = ", phi, " $$", interpret_phi(phi))
      },
      "chi2_gleich" = {
        obs <- d$obs_chi2_gleich; k <- d$k_chi2_gleich; n <- sum(obs)
        exp_val   <- n / k
        chi2_emp  <- round(sum((obs - exp_val)^2 / exp_val), 3)
        df_chi2   <- k - 1
        chi2_krit <- round(qchisq(0.95, df_chi2), 3)
        decision  <- if (chi2_emp > chi2_krit)
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " > \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " \\leq \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ \\chi^2_{emp} = ", chi2_emp, ", \\quad \\chi^2_{krit} = ", chi2_krit, " $$<br>", decision)
      },
      "chi2_verh" = {
        obs <- d$obs_chi2_verh; probs <- d$probs_verh; n <- sum(obs)
        exp_vals  <- n * probs
        chi2_emp  <- round(sum((obs - exp_vals)^2 / exp_vals), 3)
        df_chi2   <- length(obs) - 1
        chi2_krit <- round(qchisq(0.95, df_chi2), 3)
        decision  <- if (chi2_emp > chi2_krit)
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " > \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " \\leq \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ \\chi^2_{emp} = ", chi2_emp, ", \\quad \\chi^2_{krit} = ", chi2_krit, " $$<br>", decision)
      },
      "chi2_2d" = {
        obs <- d$obs_chi2_2d
        row_tots <- rowSums(obs); col_tots <- colSums(obs); n <- sum(obs)
        exp_mat   <- outer(row_tots, col_tots) / n
        chi2_emp  <- round(sum((obs - exp_mat)^2 / exp_mat), 3)
        df_chi2   <- (nrow(obs) - 1) * (ncol(obs) - 1)
        chi2_krit <- round(qchisq(0.95, df_chi2), 3)
        decision  <- if (chi2_emp > chi2_krit)
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " > \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) verworfen. X und Y sind abh\u00e4ngig.")
        else
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " \\leq \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) beibehalten. X und Y sind unabh\u00e4ngig.")
        paste0("L\u00f6sung: $$ \\chi^2_{emp} = ", chi2_emp, ", \\quad \\chi^2_{krit} = ", chi2_krit, " $$<br>", decision)
      },
      "chi2_4feld" = {
        obs <- d$obs_chi2_4feld
        a <- obs[1,1]; b <- obs[1,2]; c <- obs[2,1]; dd <- obs[2,2]; N <- sum(obs)
        chi2_emp  <- round(N * (a*dd - b*c)^2 / ((a+b)*(c+dd)*(a+c)*(b+dd)), 3)
        chi2_krit <- round(qchisq(0.95, 1), 3)
        decision  <- if (chi2_emp > chi2_krit)
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " > \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) verworfen. X und Y sind abh\u00e4ngig.")
        else
          paste0("Da \\(\\chi^2_{emp} = ", chi2_emp, " \\leq \\chi^2_{krit} = ", chi2_krit, "\\), wird die \\(H_0\\) beibehalten. X und Y sind unabh\u00e4ngig.")
        paste0("L\u00f6sung: $$ \\chi^2_{emp} = ", chi2_emp, ", \\quad \\chi^2_{krit} = ", chi2_krit, " $$<br>", decision)
      },
      "lm"           = paste("$$ y_{i}=", round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2), "+", round(cov(x, y) / var(x), 2), "\\cdot x_{i}", "$$"),
      "lm_predict" = {
        a_val  <- round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2)
        b_val  <- round(cov(x, y) / var(x), 2)
        x_pred <- d$x_pred
        y_hat  <- round(a_val + b_val * x_pred, 2)
        paste("L\u00f6sung: $$ \\hat{y} =", y_hat, "$$")
      },
      "lm_std" = {
        b_val <- round(cov(x, y) / var(x), 2)
        sx    <- round(sqrt(var(x)), 2)
        sy    <- round(sqrt(var(y)), 2)
        beta  <- round(b_val * sx / sy, 3)
        paste("L\u00f6sung: $$ \\beta =", beta, "$$")
      },
      "lm_sig" = {
        b_val  <- round(cov(x, y) / var(x), 2)
        sx     <- round(sqrt(var(x)), 2); sy <- round(sqrt(var(y)), 2)
        sigma_hat <- round(sqrt((n * sy^2 - n * b_val^2 * sx^2) / (n - 2)), 3)
        s_b    <- round(sigma_hat / (sx * sqrt(n)), 3)
        t_emp  <- round(b_val / s_b, 2)
        df_val <- n - 2
        t_krit <- round(qt(0.975, df_val), 3)
        decision <- if (abs(t_emp) > t_krit)
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "lm_sig2" = {
        b_val  <- round(cov(x, y) / var(x), 2)
        sx     <- round(sqrt(var(x)), 2); sy <- round(sqrt(var(y)), 2)
        sigma_hat <- round(sqrt((n * sy^2 - n * b_val^2 * sx^2) / (n - 2)), 3)
        s_b    <- round(sigma_hat / (sx * sqrt(n)), 3)
        t_emp  <- round(b_val / s_b, 2)
        df_val <- n - 2
        t_krit <- round(qt(0.95, df_val), 3)
        decision <- if (t_emp > t_krit)
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad df = ", df_val, " $$<br>", decision)
      },
      "t.test1" = {
        n_val  <- length(x)
        xbar   <- round(mean(x), 2)
        s_val  <- round(sqrt(var(x)), 2)
        se_val <- round(s_val / sqrt(n_val), 2)
        t_emp  <- round((xbar - d$mu0) / se_val, 2)
        df_val <- n_val - 1
        t_krit <- round(qt(0.975, df_val), 3)
        p_val  <- round(2 * pt(-abs(t_emp), df_val), 3)
        decision <- if (abs(t_emp) > t_krit)
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "t.test1_2" = {
        n_val  <- length(x)
        xbar   <- round(mean(x), 2)
        s_val  <- round(sqrt(var(x)), 2)
        se_val <- round(s_val / sqrt(n_val), 2)
        t_emp  <- round((xbar - d$mu0) / se_val, 2)
        df_val <- n_val - 1
        t_krit <- round(qt(0.95, df_val), 3)
        p_val  <- round(pt(t_emp, df_val, lower.tail = FALSE), 3)
        decision <- if (t_emp > t_krit)
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        else
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "t.test" = {
        df_t_ind <- length(x_g1) + length(x_g2) - 2
        t_emp  <- round(t.test(x_g1, x_g2)$statistic, 2)
        p_val  <- round(t.test(x_g1, x_g2)$p.value, 3)
        t_krit <- round(qt(0.975, df_t_ind), 3)
        decision <- if (abs(t_emp) > t_krit) {
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        } else {
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        }
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "t.test2" = {
        df_t_ind <- length(x_g1) + length(x_g2) - 2
        t_emp  <- round(t.test(x_g1, x_g2)$statistic, 2)
        p_val  <- round(t.test(x_g1, x_g2, alternative = "greater")$p.value, 3)
        t_krit <- round(qt(0.95, df_t_ind), 3)
        decision <- if (t_emp > t_krit) {
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        } else {
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        }
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "abh_t.test" = {
        df_dep <- nrow(df_t) - 1
        t_emp  <- round(round(mean(diff), 2) / round(round(sqrt(var(diff)), 2) / round(sqrt(nrow(df_t)), 2), 2), 2)
        p_val  <- round(t.test(df_t$t0, df_t$t1, paired = TRUE)$p.value, 3)
        t_krit <- round(qt(0.975, df_dep), 3)
        decision <- if (abs(t_emp) > t_krit) {
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        } else {
          paste0("Da \\(|t_{emp}| = ", abs(t_emp), " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        }
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "abh_t.test2" = {
        df_dep <- nrow(df_t) - 1
        t_emp  <- round(round(mean(diff), 2) / round(round(sqrt(var(diff)), 2) / round(sqrt(nrow(df_t)), 2), 2), 2)
        p_val  <- round(t.test(df_t$t0, df_t$t1, paired = TRUE, alternative = "greater")$p.value, 3)
        t_krit <- round(qt(0.95, df_dep), 3)
        decision <- if (t_emp > t_krit) {
          paste0("Da \\(t_{emp} = ", t_emp, " > t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) verworfen.")
        } else {
          paste0("Da \\(t_{emp} = ", t_emp, " \\leq t_{krit} = ", t_krit, "\\), wird die \\(H_0\\) beibehalten.")
        }
        paste0("L\u00f6sung: $$ t_{emp} = ", t_emp, ", \\quad p = ", p_val, " $$<br>", decision)
      },
      "cohend" = {
        d_val <- round(effsize::cohen.d(x_g1, x_g2)$estimate, 2)
        paste0("L\u00f6sung: $$ d=", d_val, " $$", interpret_cohend(d_val))
      },
      "rm_anova" = {
        rm   <- d$df_rm; n_s <- d$n_rm; k <- 3
        t0 <- rm$t0; t1 <- rm$t1; t2 <- rm$t2
        gm   <- round(mean(c(t0, t1, t2)), 2)
        mt0  <- round(mean(t0), 2); mt1 <- round(mean(t1), 2); mt2 <- round(mean(t2), 2)
        person_means <- round(rowMeans(rm[, c("t0","t1","t2")]), 2)
        var_sys    <- round(n_s * ((mt0-gm)^2 + (mt1-gm)^2 + (mt2-gm)^2) / (k-1), 3)
        time_idx   <- rep(1:k, each = n_s)
        pers_idx   <- rep(1:n_s, k)
        ai_vals    <- c(mt0, mt1, mt2)[time_idx]
        pm_vals    <- person_means[pers_idx]
        qs_resid   <- round(sum((c(t0,t1,t2) - (ai_vals + pm_vals - gm))^2), 3)
        df_time    <- k - 1
        df_error   <- (k - 1) * (n_s - 1)
        var_res    <- round(qs_resid / df_error, 3)
        f_emp      <- round(var_sys / var_res, 2)
        f_crit     <- round(qf(0.95, df_time, df_error), 2)
        decision <- if (f_emp > f_crit)
          paste0("Da \\(F_{emp} = ", f_emp, " > F_{krit} = ", f_crit, "\\), wird die \\(H_0\\) verworfen. ",
                 "Es besteht ein signifikanter Unterschied zwischen den Messzeitpunkten.")
        else
          paste0("Da \\(F_{emp} = ", f_emp, " \\leq F_{krit} = ", f_crit, "\\), wird die \\(H_0\\) beibehalten. ",
                 "Es besteht kein signifikanter Unterschied zwischen den Messzeitpunkten.")
        paste0("L\u00f6sung: $$ F_{emp} = ", f_emp, ", \\quad F_{krit} = ", f_crit, " $$<br>", decision)
      },
      "oneway_anova" = {
        all_3   <- c(d$x_3g1, d$x_3g2, d$x_3g3)
        n_per   <- length(d$x_3g1)
        m1 <- round(mean(d$x_3g1), 2); m2 <- round(mean(d$x_3g2), 2); m3 <- round(mean(d$x_3g3), 2)
        v1 <- round(var(d$x_3g1), 2);  v2 <- round(var(d$x_3g2), 2);  v3 <- round(var(d$x_3g3), 2)
        grand_mean <- round(mean(all_3), 2)
        res_var <- round((v1 + v2 + v3) / 3, 2)
        sys_var <- round(n_per * round((m1 - grand_mean)^2 + (m2 - grand_mean)^2 + (m3 - grand_mean)^2, 2) / 2, 2)
        f_emp   <- round(sys_var / res_var, 2)
        df2_anova <- 3 * (n_per - 1)
        f_crit  <- round(qf(p = .95, df1 = 2, df2 = df2_anova), 2)
        decision <- if (f_emp > f_crit) {
          paste0("Da \\(F_{emp} = ", f_emp, " > F_{krit} = ", f_crit, "\\), wird die \\(H_0\\) verworfen. ",
                 "Es besteht ein signifikanter Mittelwertsunterschied zwischen den Gruppen.")
        } else {
          paste0("Da \\(F_{emp} = ", f_emp, " \\leq F_{krit} = ", f_crit, "\\), wird die \\(H_0\\) beibehalten. ",
                 "Es besteht kein signifikanter Mittelwertsunterschied zwischen den Gruppen.")
        }
        paste0("L\u00f6sung: $$ F_{emp} = ", f_emp, ", \\quad F_{krit} = ", f_crit, " $$",
               "<br>", decision)
      },
      "confint_mean_with_var" = paste("L\u00f6sung: $$\\bar{x}=", round(mean(x), 2), "; KI_{95\\%}=",
                                      round(mean(x), 2) - round(1.96 * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), ", ",
                                      round(mean(x), 2) + round(1.96 * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$"),
      "confint_mean_no_var"   = paste("L\u00f6sung: $$\\bar{x}=", round(mean(x), 2), "; KI_{95\\%}=",
                                      round(mean(x), 2) - round(round(qt(0.975, length(x) - 1), 3) * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), ", ",
                                      round(mean(x), 2) + round(round(qt(0.975, length(x) - 1), 3) * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$"),
      NULL
    )
  })

  loesungsweg_reactive <- eventReactive(input$solve, {
    d      <- sim_data()
    x      <- d$x; y <- d$y; n <- d$n
    x_g1   <- d$x_g1; x_g2 <- d$x_g2
    x_rank <- d$x_rank; y_rank <- d$y_rank
    df_t   <- d$df_t; diff <- d$diff
    x_3g1  <- d$x_3g1; x_3g2 <- d$x_3g2; x_3g3 <- d$x_3g3; n3 <- d$n3

    switch(input$drop,

      "modalwert" = {
        freq_tbl <- sort(table(x), decreasing = TRUE)
        x_mod    <- as.numeric(names(freq_tbl)[1])
        freq_str <- paste(paste0(names(freq_tbl), " (", as.numeric(freq_tbl), "x)"), collapse = ", ")
        paste0("L\u00f6sungsweg: H\u00e4ufigkeiten: ", freq_str,
               " $$ x_{mod} = ", x_mod, " $$")
      },

      "mean" = paste("L\u00f6sungsweg:", latex_mean_steps(x, n)),

      "median" = {
        xs <- sort(x)
        if ((length(x) %% 2) == 0) {
          paste("L\u00f6sungweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$\n",
                "$$\\textit{Md} = \\frac{x_{(\\frac{", n, "}{2})}+x_{(\\frac{", n, "}{2}+1)}}{2}$$")
        } else {
          paste("L\u00f6sungweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$\n",
                "$$\\textit{Md} = x_{\\frac{", n, "+1}{2}}$$")
        }
      },

      "spannweite" = {
        xs <- sort(x)
        paste("L\u00f6sungsweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$",
              "$$ x_{min} =", min(x), ", \\quad x_{max} =", max(x), "$$",
              "$$ R = x_{max} - x_{min} =", max(x), "-", min(x), "=", max(x) - min(x), "$$")
      },

      "var" = paste("L\u00f6sungsweg:", latex_mean_steps(x, n), "\n", latex_var_steps(x, n)),

      "sd" = paste("L\u00f6sungsweg:", latex_mean_steps(x, n), "\n", latex_var_steps(x, n), "\n",
                   latex_sd_result(x), "\n",
                   "$$ s=", round(sqrt(var(x)), 2), "$$"),

      "iqr" = {
        xs <- sort(x)
        if ((n * 0.25) %% 1 == 0) {
          paste("L\u00f6sungsweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$",
                "$$ \\text{Unteres Quartil }", "(\\alpha=0.25)", ": $$",
                "\n", "$$ n \\cdot \\alpha =", n, "\\cdot", 0.25, "=", n * 0.25, "$$",
                "$$ \\rightarrow n \\cdot \\alpha  \\text{ ist eine ganze Zahl. $$", "\n",
                "$$ l = ", n * 0.25, "} $$", "\n",
                "$$ \\tilde{x}_{0.25} = \\frac{x_{(l)} + x_{(l+1)}}{2} = \\frac{x_{(", n * 0.25, ")} + x_{(", n * 0.25 + 1, ")}}{2} = \\frac{", xs[n * 0.25], "+", xs[n * 0.25 + 1], "}{2} =", (xs[n * 0.25] + xs[n * 0.25 + 1]) / 2, "$$", "\n",
                "$$ \\text{Oberes Quartil }", "(\\alpha=0.75)", ": $$", "\n",
                "$$ n \\cdot \\alpha =", n, "\\cdot", 0.75, "=", n * 0.75, "$$",
                "$$ \\rightarrow n \\cdot \\alpha  \\text{ ist eine ganze Zahl. $$", "\n",
                "$$ l = ", n * 0.75, "} $$", "\n",
                "$$ \\tilde{x}_{0.75} = \\frac{x_{(l)} + x_{(l+1)}}{2} = \\frac{x_{(", n * 0.75, ")} + x_{(", n * 0.75 + 1, ")}}{2} = \\frac{", xs[n * 0.75], "+", xs[n * 0.75 + 1], "}{2} =", (xs[n * 0.75] + xs[n * 0.75 + 1]) / 2, "$$", "\n",
                "$$ IQR = \\tilde{x}_{0.75} - \\tilde{x}_{0.25} = ", (xs[n * 0.75] + xs[n * 0.75 + 1]) / 2, "-", (xs[n * 0.25] + xs[n * 0.25 + 1]) / 2, "=", (xs[n * 0.75] + xs[n * 0.75 + 1]) / 2 - (xs[n * 0.25] + xs[n * 0.25 + 1]) / 2, "$$")
        } else {
          paste("L\u00f6sungweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$",
                "$$ \\text{Unteres Quartil }", "(\\alpha=0.25)", ": $$", "\n",
                "$$ n \\cdot \\alpha =", n, "\\cdot", 0.25, "=", n * 0.25, "$$",
                "$$ \\rightarrow n \\cdot \\alpha  \\text{ ist keine ganze Zahl.} $$", "\n",
                "$$ l = ", ceiling(n * 0.25), " $$", "\n",
                "$$  \\tilde{x}_{0.25} = x_{(", ceiling(n * 0.25), ")} = ", xs[ceiling(n * 0.25)], "  $$", "\n",
                "$$ \\text{Oberes Quartil }", "(\\alpha=0.75)", ": $$", "\n",
                "$$ n \\cdot \\alpha =", n, "\\cdot", 0.75, "=", n * 0.75, "$$",
                "$$ \\rightarrow n \\cdot \\alpha  \\text{ ist keine ganze Zahl.} $$", "\n",
                "$$ l = ", ceiling(n * 0.75), " $$", "\n",
                "$$  \\tilde{x}_{0.75} = x_{(", ceiling(n * 0.75), ")} = ", xs[ceiling(n * 0.75)], "  $$", "\n",
                "$$ IQR = \\tilde{x}_{0.75} - \\tilde{x}_{0.25} = ", xs[ceiling(n * 0.75)], "-", xs[ceiling(n * 0.25)], "=", xs[ceiling(n * 0.75)] - xs[ceiling(n * 0.25)], "$$")
        }
      },

      "boxplot" = {
        xs <- sort(x)
        # Median
        md <- if (n %% 2 == 0) (xs[n/2] + xs[n/2+1]) / 2 else xs[(n+1)/2]
        md_step <- if (n %% 2 == 0)
          paste0("$$ \\tilde{x}_{0.50} = \\frac{x_{(", n/2, ")}+x_{(", n/2+1, ")}}{2} = \\frac{", xs[n/2], "+", xs[n/2+1], "}{2} = ", md, " $$")
        else
          paste0("$$ \\tilde{x}_{0.50} = x_{(", (n+1)/2, ")} = ", md, " $$")
        # Quartile
        if ((n * 0.25) %% 1 == 0) {
          q1 <- (xs[n*0.25] + xs[n*0.25+1]) / 2
          q3 <- (xs[n*0.75] + xs[n*0.75+1]) / 2
          q1_step <- paste0("$$ \\tilde{x}_{0.25} = \\frac{x_{(", n*0.25, ")}+x_{(", n*0.25+1, ")}}{2} = \\frac{", xs[n*0.25], "+", xs[n*0.25+1], "}{2} = ", q1, " $$")
          q3_step <- paste0("$$ \\tilde{x}_{0.75} = \\frac{x_{(", n*0.75, ")}+x_{(", n*0.75+1, ")}}{2} = \\frac{", xs[n*0.75], "+", xs[n*0.75+1], "}{2} = ", q3, " $$")
        } else {
          q1 <- xs[ceiling(n*0.25)]
          q3 <- xs[ceiling(n*0.75)]
          q1_step <- paste0("$$ \\tilde{x}_{0.25} = x_{(", ceiling(n*0.25), ")} = ", q1, " $$")
          q3_step <- paste0("$$ \\tilde{x}_{0.75} = x_{(", ceiling(n*0.75), ")} = ", q3, " $$")
        }
        iqr_val  <- q3 - q1
        fl       <- q1 - 1.5 * iqr_val
        fh       <- q3 + 1.5 * iqr_val
        w_low    <- min(xs[xs >= fl])
        w_high   <- max(xs[xs <= fh])
        outliers <- xs[xs < fl | xs > fh]
        out_str  <- if (length(outliers) > 0) paste0("$$ \\text{Ausrei\u00dfer: } ", paste(outliers, collapse = ", "), " $$") else ""
        paste0("L\u00f6sungsweg: $$x_{sortiert}=", paste(xs, collapse = ";"), "$$",
               "\n", md_step,
               "\n$$ \\text{Unteres Quartil:} $$\n", q1_step,
               "\n$$ \\text{Oberes Quartil:} $$\n", q3_step,
               "\n$$ IQR = ", q3, " - ", q1, " = ", iqr_val, " $$",
               "\n$$ \\text{Untere Whiskergrenze: } \\tilde{x}_{0.25} - 1.5 \\cdot IQR = ", q1, " - ", 1.5*iqr_val, " = ", fl, " $$",
               "\n$$ W_{unten} = ", w_low, " \\text{ (kleinster Wert } \\geq ", fl, ") $$",
               "\n$$ \\text{Obere Whiskergrenze: } \\tilde{x}_{0.75} + 1.5 \\cdot IQR = ", q3, " + ", 1.5*iqr_val, " = ", fh, " $$",
               "\n$$ W_{oben} = ", w_high, " \\text{ (gr\u00f6\u00dfter Wert } \\leq ", fh, ") $$",
               if (nchar(out_str) > 0) paste0("\n", out_str) else "")
      },

      "scale" = paste("L\u00f6sungsweg:", latex_mean_steps(x, n), "\n", latex_var_steps(x, n), "\n",
                      latex_sd_result(x), "\n",
                      "$$ s=", round(sqrt(var(x)), 2), "$$", "\n",
                      "$$ z_i=", paste(as.character(round(scale(x), 2)), collapse = "; "), "$$"),

      "zdist" = {
        pos   <- d$zdist_pos
        z_pos <- round(scale(x), 2)[pos]
        p_val   <- if (d$zdist_direction == "lower") round(pnorm(z_pos), 4) else round(1 - pnorm(z_pos), 4)
        p_step  <- if (d$zdist_direction == "lower") {
          paste0("$$ P(X \\leq x_{", pos, "}) = \\Phi(", z_pos, ") = ", p_val, " $$")
        } else {
          paste0("$$ P(X > x_{", pos, "}) = 1 - \\Phi(", z_pos, ") = 1 - ", round(pnorm(z_pos), 4), " = ", p_val, " $$")
        }
        paste("L\u00f6sungsweg:", latex_mean_steps(x, n), "\n", latex_var_steps(x, n), "\n",
              latex_sd_result(x), "\n",
              "$$ s=", round(sqrt(var(x)), 2), "$$", "\n",
              "$$ z_{", pos, "}=\\frac{x_{", pos, "}-\\bar{x}}{s}=\\frac{", x[pos], "-", round(mean(x), 2), "}{", round(sd(x), 2), "}=", z_pos, "$$", "\n",
              p_step)
      },

      "binom" = {
        nb <- d$n_binom; kb <- d$k_binom; pb <- d$p_binom
        binom_coeff <- choose(nb, kb)
        p_result    <- round(dbinom(kb, nb, pb), 4)
        paste0(
          "L\u00f6sungsweg: ",
          "$$ \\binom{n}{k} = \\frac{n!}{k! \\cdot (n-k)!} = \\frac{", nb, "!}{", kb, "! \\cdot ", nb - kb, "!} = ", binom_coeff, " $$",
          "$$ P(X = ", kb, ") = \\binom{", nb, "}{", kb, "} \\cdot p^k \\cdot (1-p)^{n-k} $$",
          "$$ P(X = ", kb, ") = ", binom_coeff, " \\cdot ", pb, "^{", kb, "} \\cdot ", round(1 - pb, 1), "^{", nb - kb, "} $$",
          "$$ P(X = ", kb, ") = ", binom_coeff, " \\cdot ", round(pb^kb, 4), " \\cdot ", round((1 - pb)^(nb - kb), 4), " $$",
          "$$ P(X = ", kb, ") = ", p_result, " $$"
        )
      },
      "bernoulli" = {
        pb <- d$p_bernoulli; kb <- d$k_bernoulli
        p_result <- round(pb^kb * (1 - pb)^(1 - kb), 4)
        paste0(
          "L\u00f6sungsweg: ",
          "$$ P(X = ", kb, ") = p^k \\cdot (1-p)^{1-k} $$",
          "$$ P(X = ", kb, ") = ", pb, "^{", kb, "} \\cdot ", round(1 - pb, 1), "^{", 1 - kb, "} $$",
          "$$ P(X = ", kb, ") = ", round(pb^kb, 4), " \\cdot ", round((1 - pb)^(1 - kb), 4), " $$",
          "$$ P(X = ", kb, ") = ", p_result, " $$"
        )
      },

      "cov" = paste(
        "L\u00f6sungsweg: $$\\bar{x}=", "\\frac{", paste(x, collapse = " + "), "}{", length(x), "}=", round(mean(x), 2), "$$",
        "\n",
        "$$\\bar{y}=", "\\frac{", paste(y, collapse = " + "), "}{", length(y), "}=", round(mean(y), 2), "$$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0("(", x, "-", round(mean(x), 2), ")","\\cdot", "(", y, "-", round(mean(y), 2), ")"), collapse = " + "), "}{", n, "-1} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round(x - mean(x), 2), "\\cdot", round(y - mean(y), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round((x - mean(x)) * (y - mean(y)), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", round(sum((x - mean(x)) * (y - mean(y))), 2), "}{", n - 1, "} $$"
      ),

      "cor_pearson" = paste(
        "L\u00f6sungsweg: $$ s_{xy}=\\frac{\\displaystyle \\sum_{i=1}^{n}(x_{i}-\\bar{x})\\cdot(y_{i}-\\bar{y})}{n-1} $$",
        "\n",
        "$$ s_{x}=\\sqrt{\\frac{\\displaystyle\\sum_{i=1}^{n}(x_{i}-\\bar{x})^2}{n-1}} $$",
        "\n",
        "$$ s_{y}=\\sqrt{\\frac{\\displaystyle\\sum_{i=1}^{n}(y_{i}-\\bar{y})^2}{n-1}} $$",
        "\n",
        "$$\\bar{x}=", "\\frac{", paste(x, collapse = " + "), "}{", length(x), "}=", round(mean(x), 2), "$$",
        "\n",
        "$$\\bar{y}=", "\\frac{", paste(y, collapse = " + "), "}{", length(y), "}=", round(mean(y), 2), "$$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0("(", x, "-", round(mean(x), 2), ")","\\cdot", "(", y, "-", round(mean(y), 2), ")"), collapse = " + "), "}{", n, "-1} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round(x - mean(x), 2), "\\cdot", round(y - mean(y), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round((x - mean(x)) * (y - mean(y)), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", round(sum((x - mean(x)) * (y - mean(y))), 2), "}{", n - 1, "}=", round(cov(x, y), 2), "$$",
        "\n",
        "$$ s_{x}=\\sqrt{\\frac{\\displaystyle", paste(paste0("(", x, "-", round(mean(x), 2), ")^2"), collapse = " + "), "}{", n, "-1}}=", round(sd(x), 2), "$$",
        "\n",
        "$$ s_{y}=\\sqrt{\\frac{\\displaystyle", paste(paste0("(", y, "-", round(mean(y), 2), ")^2"), collapse = " + "), "}{", n, "-1}}=", round(sd(y), 2), "$$",
        "\n",
        "$$r = \\dfrac{", round(cov(x, y), 2), "}{", round(sd(x), 2), "\\cdot", round(sd(y), 2), "}$$"
      ),

      "cor_spearman" = paste(
        "L\u00f6sungsweg: $$ r_s={}1-\\frac{6 \\cdot (\\displaystyle", paste(paste0("(", x_rank, "-", y_rank, ")^2"), collapse = " + "), ")}{", n, " \\cdot (", n^2, "- 1)} $$",
        "\n",
        "$$ r_s={}1-\\frac{6 \\cdot (\\displaystyle", paste((x_rank - y_rank)^2, collapse = " + "), ")}{", n * (n^2 - 1), "} $$",
        "\n",
        "$$ r_s={}1-\\frac{6 \\cdot \\displaystyle", sum((x_rank - y_rank)^2), "}{", n * (n^2 - 1), "} $$",
        "\n",
        "$$ r_s={}1-\\frac{", 6 * sum((x_rank - y_rank)^2), "}{", n * (n^2 - 1), "} $$",
        "\n",
        "$$ r_s=1-", round(6 * sum((x_rank - y_rank)^2) / (n * (n^2 - 1)), 2), " $$"
      ),

      "cor_sig" = ,
      "cor_sig2" = {
        two_sided <- input$drop == "cor_sig"
        r_val  <- round(cor(x, y), 2)
        df_val <- n - 2
        t_emp  <- round(r_val * sqrt(df_val) / sqrt(1 - r_val^2), 2)
        t_krit <- round(qt(if (two_sided) 0.975 else 0.95, df_val), 3)
        paste0("L\u00f6sungsweg: ",
               "$$ t_{emp} = \\frac{r \\cdot \\sqrt{N-2}}{\\sqrt{1-r^2}} = ",
               "\\frac{", r_val, " \\cdot \\sqrt{", df_val, "}}{\\sqrt{1-", r_val, "^2}} = ",
               "\\frac{", round(r_val * sqrt(df_val), 3), "}{\\sqrt{", round(1 - r_val^2, 3), "}} = ",
               "\\frac{", round(r_val * sqrt(df_val), 3), "}{", round(sqrt(1 - r_val^2), 3), "} = ", t_emp, " $$",
               "$$ df = N - 2 = ", df_val, " $$",
               "$$ t_{krit(df=", df_val, ", \\alpha=.05)} = ", t_krit, " $$")
      },

      "cor_sig_s" = ,
      "cor_sig_s2" = {
        two_sided <- input$drop == "cor_sig_s"
        rs_val <- round(1 - 6 * sum((x_rank - y_rank)^2) / (n * (n^2 - 1)), 2)
        df_val <- n - 2
        t_emp  <- round(rs_val * sqrt(df_val) / sqrt(1 - rs_val^2), 2)
        t_krit <- round(qt(if (two_sided) 0.975 else 0.95, df_val), 3)
        paste0("L\u00f6sungsweg: ",
               "$$ t_{emp} = \\frac{r_s \\cdot \\sqrt{N-2}}{\\sqrt{1-r_s^2}} = ",
               "\\frac{", rs_val, " \\cdot \\sqrt{", df_val, "}}{\\sqrt{1-", rs_val, "^2}} = ",
               "\\frac{", round(rs_val * sqrt(df_val), 3), "}{\\sqrt{", round(1 - rs_val^2, 3), "}} = ",
               "\\frac{", round(rs_val * sqrt(df_val), 3), "}{", round(sqrt(1 - rs_val^2), 3), "} = ", t_emp, " $$",
               "$$ df = N - 2 = ", df_val, " $$",
               "$$ t_{krit(df=", df_val, ", \\alpha=.05)} = ", t_krit, " $$")
      },

      "chi2_gleich" = {
        obs <- d$obs_chi2_gleich; k <- d$k_chi2_gleich; n <- sum(obs)
        exp_val  <- n / k
        chi2_emp <- round(sum((obs - exp_val)^2 / exp_val), 3)
        df_chi2  <- k - 1
        terms    <- paste(paste0("\\frac{(", obs, " - ", exp_val, ")^2}{", exp_val, "}"), collapse = " + ")
        paste0("L\u00f6sungsweg: ",
               "$$ E_i = \\frac{n}{k} = \\frac{", n, "}{", k, "} = ", exp_val, " $$",
               "$$ \\chi^2 = ", terms, " = ", chi2_emp, " $$",
               "$$ df = k - 1 = ", k, " - 1 = ", df_chi2, " $$",
               "$$ \\chi^2_{krit(df=", df_chi2, ", \\alpha=.05)} = ", round(qchisq(0.95, df_chi2), 3), " $$")
      },

      "chi2_verh" = {
        obs <- d$obs_chi2_verh; probs <- d$probs_verh; n <- sum(obs); k <- length(obs)
        exp_vals <- round(n * probs, 2)
        chi2_emp <- round(sum((obs - exp_vals)^2 / exp_vals), 3)
        df_chi2  <- k - 1
        exp_lines <- paste(paste0("$$ E_", 1:k, " = ", n, " \\cdot ", round(probs, 4), " = ", exp_vals, " $$"), collapse = " ")
        terms     <- paste(paste0("\\frac{(", obs, " - ", exp_vals, ")^2}{", exp_vals, "}"), collapse = " + ")
        paste0("L\u00f6sungsweg: ",
               exp_lines,
               "$$ \\chi^2 = ", terms, " = ", chi2_emp, " $$",
               "$$ df = k - 1 = ", k, " - 1 = ", df_chi2, " $$",
               "$$ \\chi^2_{krit(df=", df_chi2, ", \\alpha=.05)} = ", round(qchisq(0.95, df_chi2), 3), " $$")
      },

      "chi2_2d" = {
        obs <- d$obs_chi2_2d
        row_tots <- rowSums(obs); col_tots <- colSums(obs); n <- sum(obs)
        exp_mat  <- round(outer(row_tots, col_tots) / n, 2)
        chi2_emp <- round(sum((obs - exp_mat)^2 / exp_mat), 3)
        df_chi2  <- (nrow(obs) - 1) * (ncol(obs) - 1)

        # Randsummen-Tabelle als LaTeX array
        col_header <- paste(colnames(obs), collapse = " & ")
        data_rows  <- paste(sapply(seq_len(nrow(obs)), function(i) {
          paste0(rownames(obs)[i], " & ", paste(obs[i, ], collapse = " & "), " & ", row_tots[i])
        }), collapse = " \\\\ ")
        sum_row    <- paste0("\\sum & ", paste(col_tots, collapse = " & "), " & ", n)
        col_spec   <- paste(rep("c", ncol(obs) + 1), collapse = "|")
        randsummen_tbl <- paste0(
          "$$ \\begin{array}{c|", col_spec, "} & ", col_header, " & \\sum \\\\ \\hline ",
          data_rows, " \\\\ \\hline ", sum_row,
          " \\end{array} $$"
        )

        exp_lines <- paste(apply(expand.grid(seq_len(nrow(obs)), seq_len(ncol(obs))), 1, function(idx) {
          i <- idx[1]; j <- idx[2]
          paste0("$$ E_{", rownames(obs)[i], j, "} = \\frac{", row_tots[i], " \\cdot ", col_tots[j],
                 "}{", n, "} = ", exp_mat[i, j], " $$")
        }), collapse = " ")
        terms <- paste(apply(expand.grid(seq_len(nrow(obs)), seq_len(ncol(obs))), 1, function(idx) {
          i <- idx[1]; j <- idx[2]
          paste0("\\frac{(", obs[i, j], " - ", exp_mat[i, j], ")^2}{", exp_mat[i, j], "}")
        }), collapse = " + ")
        paste0("L\u00f6sungsweg: ",
               randsummen_tbl,
               exp_lines,
               "$$ \\chi^2 = ", terms, " = ", chi2_emp, " $$",
               "$$ df = (k-1) \\cdot (l-1) = (", nrow(obs), "-1) \\cdot (", ncol(obs), "-1) = ", df_chi2, " $$",
               "$$ \\chi^2_{krit(df=", df_chi2, ", \\alpha=.05)} = ", round(qchisq(0.95, df_chi2), 3), " $$")
      },

      "chi2_4feld" = {
        obs <- d$obs_chi2_4feld
        a <- obs[1,1]; b <- obs[1,2]; c <- obs[2,1]; dd <- obs[2,2]; N <- sum(obs)
        ad_bc     <- a*dd - b*c
        numer     <- N * ad_bc^2
        denom     <- (a+b) * (c+dd) * (a+c) * (b+dd)
        chi2_emp  <- round(numer / denom, 3)
        paste0("L\u00f6sungsweg: ",
               "$$ \\chi^2 = \\frac{N \\cdot (a \\cdot d - b \\cdot c)^2}{(a+b)(c+d)(a+c)(b+d)} $$",
               "$$ = \\frac{", N, " \\cdot (", a, " \\cdot ", dd, " - ", b, " \\cdot ", c, ")^2}",
               "{(", a, "+", b, ")(", c, "+", dd, ")(", a, "+", c, ")(", b, "+", dd, ")} $$",
               "$$ = \\frac{", N, " \\cdot (", ad_bc, ")^2}{", denom, "} $$",
               "$$ = \\frac{", numer, "}{", denom, "} = ", chi2_emp, " $$",
               "$$ df = 1 $$",
               "$$ \\chi^2_{krit(df=1, \\alpha=.05)} = ", round(qchisq(0.95, 1), 3), " $$")
      },

      "alpha_kumulierung" = {
        k <- d$k_alpha
        paste0(
          "L\u00f6sungsweg: ",
          "$$ P(\\text{mind. 1 Fehler 1. Art}) = 1 - (1 - \\alpha)^k $$",
          "$$ = 1 - (1 - 0.05)^{", k, "} $$",
          "$$ = 1 - 0.95^{", k, "} $$",
          "$$ = 1 - ", round(0.95^k, 4), " $$",
          "$$ = ", round(1 - 0.95^k, 4), " $$"
        )
      },
      "bonferroni" = {
        k <- d$k_alpha
        paste0(
          "L\u00f6sungsweg: ",
          "$$ \\alpha^* = \\frac{\\alpha}{k} $$",
          "$$ = \\frac{0.05}{", k, "} $$",
          "$$ = ", round(0.05 / k, 4), " $$"
        )
      },
      "fdr" = {
        p <- d$p_fdr_raw; k <- d$k_fdr
        p_adj <- round(p * k / seq_along(p), 4)
        sig   <- p_adj <= 0.05
        steps <- paste(sapply(seq_along(p), function(i) {
          paste0("$$ p^*_{(", i, ")} = ", p[i], " \\cdot \\frac{", k, "}{", i, "} = ", p_adj[i],
                 ifelse(sig[i], " \\leq 0.05 \\Rightarrow \\text{ signifikant}", " > 0.05 \\Rightarrow \\text{ nicht signifikant}"), " $$")
        }), collapse = " ")
        paste0("L\u00f6sungsweg: Die p-Werte werden aufsteigend sortiert und dann angepasst: ", steps)
      },
      "eta2" = {
        f <- d$eff_F; df1 <- d$eff_df1; df2 <- d$eff_df2
        numer <- round(df1 * f, 3); denom <- round(df1 * f + df2, 3)
        eta2  <- round(numer / denom, 3)
        paste0("L\u00f6sungsweg: ",
               "$$ \\eta^2 = \\frac{df_1 \\cdot F}{df_1 \\cdot F + df_2} $$",
               "$$ = \\frac{", df1, " \\cdot ", f, "}{", df1, " \\cdot ", f, " + ", df2, "} $$",
               "$$ = \\frac{", numer, "}{", denom, "} = ", eta2, " $$")
      },
      "omega2" = {
        f <- d$eff_F; df1 <- d$eff_df1; df2 <- d$eff_df2; N <- d$eff_N
        numer <- round(df1 * (f - 1), 3); denom <- round(df1 * (f - 1) + N, 3)
        omega2 <- round(numer / denom, 3)
        paste0("L\u00f6sungsweg: ",
               "$$ \\omega^2 = \\frac{df_1 \\cdot (F - 1)}{df_1 \\cdot (F - 1) + N} $$",
               "$$ = \\frac{", df1, " \\cdot (", f, " - 1)}{", df1, " \\cdot (", f, " - 1) + ", N, "} $$",
               "$$ = \\frac{", numer, "}{", denom, "} = ", omega2, " $$")
      },
      "cramersv" = {
        chi2 <- d$chi2_cramersv; N <- d$N_cramersv; k <- d$k_cramersv
        denom  <- round(N * (k - 1), 3)
        inner  <- round(chi2 / denom, 4)
        V      <- round(sqrt(chi2 / denom), 3)
        paste0("L\u00f6sungsweg: ",
               "$$ V = \\sqrt{\\frac{\\chi^2}{N \\cdot (k-1)}} $$",
               "$$ = \\sqrt{\\frac{", chi2, "}{", N, " \\cdot (", k, " - 1)}} $$",
               "$$ = \\sqrt{\\frac{", chi2, "}{", denom, "}} $$",
               "$$ = \\sqrt{", inner, "} = ", V, " $$")
      },
      "phi" = {
        chi2  <- d$chi2_phi; N <- d$N_phi
        inner <- round(chi2 / N, 4)
        phi   <- round(sqrt(chi2 / N), 3)
        paste0("L\u00f6sungsweg: ",
               "$$ \\phi = \\sqrt{\\frac{\\chi^2}{N}} $$",
               "$$ = \\sqrt{\\frac{", chi2, "}{", N, "}} $$",
               "$$ = \\sqrt{", inner, "} = ", phi, " $$")
      },

      "lm_predict" = {
        a_val  <- round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2)
        b_val  <- round(cov(x, y) / var(x), 2)
        x_pred <- d$x_pred
        bx     <- round(b_val * x_pred, 2)
        y_hat  <- round(a_val + b_val * x_pred, 2)
        paste0("L\u00f6sungsweg: $$ \\hat{y} = a + b \\cdot x $$",
               "$$ = ", a_val, " + ", b_val, " \\cdot ", x_pred, " $$",
               "$$ = ", a_val, " + ", bx, " $$",
               "$$ = ", y_hat, " $$")
      },
      "lm_sig" = ,
      "lm_sig2" = {
        two_sided <- input$drop == "lm_sig"
        p_crit    <- if (two_sided) 0.975 else 0.95
        b_val     <- round(cov(x, y) / var(x), 2)
        sx        <- round(sqrt(var(x)), 2)
        sy        <- round(sqrt(var(y)), 2)
        df_val    <- n - 2
        numer_sig <- round(n * sy^2 - n * b_val^2 * sx^2, 3)
        sigma_hat <- round(sqrt(numer_sig / df_val), 3)
        s_b       <- round(sigma_hat / (sx * sqrt(n)), 3)
        t_emp     <- round(b_val / s_b, 2)
        t_krit    <- round(qt(p_crit, df_val), 3)
        paste0("L\u00f6sungsweg: ",
               "$$ s_x = \\sqrt{\\dfrac{\\displaystyle",
               paste(paste0("(", x, "-", round(mean(x), 2), ")^2"), collapse = " + "),
               "}{", n, "-1}} = ", sx, " $$",
               "$$ s_y = \\sqrt{\\dfrac{\\displaystyle",
               paste(paste0("(", y, "-", round(mean(y), 2), ")^2"), collapse = " + "),
               "}{", n, "-1}} = ", sy, " $$",
               "$$ \\hat{\\sigma}_{(y|x)} = \\sqrt{\\frac{", n, " \\cdot ", sy, "^2 - ", n,
               " \\cdot ", b_val, "^2 \\cdot ", sx, "^2}{", n, "-2}} ",
               "= \\sqrt{\\frac{", numer_sig, "}{", df_val, "}} = ", sigma_hat, " $$",
               "$$ s_b = \\frac{", sigma_hat, "}{", sx, " \\cdot \\sqrt{", n, "}} = ",
               round(sigma_hat / sqrt(n), 3), " / ", sx, " = ", s_b, " $$",
               "$$ t_{emp} = \\frac{b}{s_b} = \\frac{", b_val, "}{", s_b, "} = ", t_emp, " $$",
               "$$ df = n - 2 = ", df_val, " $$",
               "$$ t_{krit(", df_val, ",", p_crit, ")} = ", t_krit, " $$")
      },

      "lm_std" = {
        b_val <- round(cov(x, y) / var(x), 2)
        sx    <- round(sqrt(var(x)), 2)
        sy    <- round(sqrt(var(y)), 2)
        beta  <- round(b_val * sx / sy, 3)
        paste0("L\u00f6sungsweg: ",
               "$$ s_x = \\sqrt{\\dfrac{\\displaystyle",
               paste(paste0("(", x, "-", round(mean(x), 2), ")^2"), collapse = " + "),
               "}{", n, "-1}} = ", sx, " $$",
               "$$ s_y = \\sqrt{\\dfrac{\\displaystyle",
               paste(paste0("(", y, "-", round(mean(y), 2), ")^2"), collapse = " + "),
               "}{", n, "-1}} = ", sy, " $$",
               "$$ \\beta = b \\cdot \\frac{s_x}{s_y} = ",
               b_val, " \\cdot \\frac{", sx, "}{", sy, "} = ",
               b_val, " \\cdot ", round(sx / sy, 4), " = ", beta, " $$")
      },

      "lm" = paste(
        "L\u00f6sungsweg: $$ s_{xy}=\\frac{\\displaystyle \\sum_{i=1}^{n}(x_{i}-\\bar{x})\\cdot(y_{i}-\\bar{y})}{n-1} $$",
        "\n",
        "$$ s_{x}=\\sqrt{\\frac{\\displaystyle\\sum_{i=1}^{n}(x_{i}-\\bar{x})^2}{n-1}} $$",
        "\n",
        "$$ s_{y}=\\sqrt{\\frac{\\displaystyle\\sum_{i=1}^{n}(y_{i}-\\bar{y})^2}{n-1}} $$",
        "\n",
        "$$\\bar{x}=", "\\frac{", paste(x, collapse = " + "), "}{", length(x), "}=", round(mean(x), 2), "$$",
        "\n",
        "$$\\bar{y}=", "\\frac{", paste(y, collapse = " + "), "}{", length(y), "}=", round(mean(y), 2), "$$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0("(", x, "-", round(mean(x), 2), ")", "\\cdot", "(", y, "-", round(mean(y), 2), ")"), collapse = " + "), "}{", n, "-1} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round(x - mean(x), 2), "\\cdot", round(y - mean(y), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", paste(paste0(round((x - mean(x)) * (y - mean(y)), 2)), collapse = " + "), "}{", n - 1, "} $$",
        "\n",
        "$$ s_{xy}=\\frac{\\displaystyle ", round(sum((x - mean(x)) * (y - mean(y))), 2), "}{", n - 1, "}=", round(cov(x, y), 2), "$$",
        "\n",
        "$$ s^2_{x}=\\dfrac{\\displaystyle", paste(paste0("(", x, "-", round(mean(x), 2), ")^2"), collapse = " + "), "}{", n, "-1}=", round(var(x), 2), "$$",
        "\n",
        "$$ b = \\dfrac{", round(cov(x, y), 2), "}{", round(var(x), 2), "}=", round(cov(x, y) / var(x), 2), "$$",
        "$$ a =", round(mean(y), 2), "-", round(cov(x, y) / var(x), 2), "\\cdot", round(mean(x), 2), "=", round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2), "$$"
      ),

      "t.test1"     = latex_onesample_ttest_loesungsweg(x, d$mu0, two_sided = TRUE),
      "t.test1_2"   = latex_onesample_ttest_loesungsweg(x, d$mu0, two_sided = FALSE),
      "t.test"      = latex_indep_ttest_loesungsweg(x_g1, x_g2, two_sided = TRUE),
      "t.test2"     = latex_indep_ttest_loesungsweg(x_g1, x_g2, two_sided = FALSE),
      "abh_t.test"  = latex_dep_ttest_loesungsweg(diff, df_t, two_sided = TRUE),
      "abh_t.test2" = latex_dep_ttest_loesungsweg(diff, df_t, two_sided = FALSE),

      "cohend" = paste(
        "L\u00f6sungsweg: $$\\bar{x_{A}}=", "\\frac{", paste(x_g1, collapse = " + "), "}{", length(x_g1), "}=", round(mean(x_g1), 2), "$$",
        "\n",
        "$$\\bar{x_{B}}=", "\\frac{", paste(x_g2, collapse = " + "), "}{", length(x_g2), "}=", round(mean(x_g2), 2), "$$",
        "\n",
        "$$ s_{A}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_g1, "-", round(mean(x_g1), 2), ")^2"), collapse = " + "), "}{", length(x_g1), "-1}=", round(var(x_g1), 2), "$$",
        "\n",
        "$$ s_{B}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_g2, "-", round(mean(x_g2), 2), ")^2"), collapse = " + "), "}{", length(x_g2), "-1}=", round(var(x_g2), 2), "$$",
        "\n",
        "$$ d=\\frac{", round(mean(x_g1), 2), "-", round(mean(x_g2), 2), "}{\\sqrt{\\frac{", round(var(x_g1), 2), "+", round(var(x_g2), 2), "}{", 2, "}}} $$",
        "\n",
        "$$ d=\\frac{", round(mean(x_g1) - mean(x_g2), 2), "}{\\sqrt{\\frac{", round(var(x_g1) + var(x_g2), 2), "}{", 2, "}}}", "$$"
      ),

      "rm_anova" = {
        rm <- d$df_rm; n_s <- d$n_rm; k <- 3
        t0 <- rm$t0; t1 <- rm$t1; t2 <- rm$t2
        gm   <- round(mean(c(t0, t1, t2)), 2)
        mt0  <- round(mean(t0), 2); mt1 <- round(mean(t1), 2); mt2 <- round(mean(t2), 2)
        person_means <- round(rowMeans(rm[, c("t0","t1","t2")]), 2)
        var_sys    <- round(n_s * ((mt0-gm)^2 + (mt1-gm)^2 + (mt2-gm)^2) / (k-1), 3)
        time_idx   <- rep(1:k, each = n_s)
        pers_idx   <- rep(1:n_s, k)
        ai_vals    <- c(mt0, mt1, mt2)[time_idx]
        pm_vals    <- person_means[pers_idx]
        qs_resid   <- round(sum((c(t0,t1,t2) - (ai_vals + pm_vals - gm))^2), 3)
        df_time    <- k - 1; df_error <- (k - 1) * (n_s - 1)
        var_res    <- round(qs_resid / df_error, 3)
        f_emp      <- round(var_sys / var_res, 2)
        f_crit     <- round(qf(0.95, df_time, df_error), 2)
        paste(
          "L\u00f6sungsweg:",
          "$$\\bar{x}_{t_0}=\\frac{", paste(t0, collapse = "+"), "}{", n_s, "}=", mt0, "$$",
          "\n",
          "$$\\bar{x}_{t_1}=\\frac{", paste(t1, collapse = "+"), "}{", n_s, "}=", mt1, "$$",
          "\n",
          "$$\\bar{x}_{t_2}=\\frac{", paste(t2, collapse = "+"), "}{", n_s, "}=", mt2, "$$",
          "\n",
          "$$\\bar{G}=\\frac{", paste(c(t0, t1, t2), collapse = "+"), "}{", n_s * k, "}=", gm, "$$",
          "\n",
          "$$\\bar{P}_m=",
          paste(paste0("\\frac{", t0, "+", t1, "+", t2, "}{3}=", person_means), collapse = ";\\quad "), "$$",
          "\n",
          "$$\\hat{\\sigma}_A^2 = \\frac{n \\cdot \\sum_i(\\bar{A}_i - \\bar{G})^2}{p-1} =",
          "\\frac{", n_s, " \\cdot (", round((mt0-gm)^2,3), "+", round((mt1-gm)^2,3), "+", round((mt2-gm)^2,3), ")}{2}=", var_sys, "$$",
          "\n",
          "$$\\hat{\\sigma}_{A \\times Vpn}^2 = \\frac{\\sum_i \\sum_m [x_{im}-(\\bar{A}_i+\\bar{P}_m-\\bar{G})]^2}{(p-1)(n-1)} =",
          "\\frac{", qs_resid, "}{2 \\cdot", n_s-1, "}=\\frac{", qs_resid, "}{", df_error, "}=", var_res, "$$",
          "\n",
          "$$df_A=p-1=2, \\quad df_{A \\times Vpn}=(p-1)(n-1)=2 \\cdot", n_s-1, "=", df_error, "$$",
          "\n",
          "$$F_{emp}=\\frac{\\hat{\\sigma}_A^2}{\\hat{\\sigma}_{A \\times Vpn}^2}=\\frac{", var_sys, "}{", var_res, "}=", f_emp, "$$",
          "\n",
          "$$F_{krit(df_1=2,\\, df_2=", df_error, ",\\, \\alpha=.05)}=", f_crit, "$$"
        )
      },

      "oneway_anova" = {
        all_3      <- c(x_3g1, x_3g2, x_3g3)
        n_per      <- length(x_3g1)
        grand_mean <- round(mean(all_3), 2)
        m1 <- round(mean(x_3g1), 2); m2 <- round(mean(x_3g2), 2); m3 <- round(mean(x_3g3), 2)
        v1 <- round(var(x_3g1), 2);  v2 <- round(var(x_3g2), 2);  v3 <- round(var(x_3g3), 2)
        res_var <- round((v1 + v2 + v3) / 3, 2)
        sys_var <- round(n_per * round((m1 - grand_mean)^2 + (m2 - grand_mean)^2 + (m3 - grand_mean)^2, 2) / 2, 2)
        f_emp   <- round(sys_var / res_var, 2)
        df2_anova <- 3 * (n_per - 1)
        f_crit    <- round(qf(p = .95, df1 = 2, df2 = df2_anova), 2)
        paste(
          "L\u00f6sungsweg: $$\\bar{x_{A}}=", "\\frac{", paste(x_3g1, collapse = " + "), "}{", n_per, "}=", m1, "$$",
          "\n",
          "$$\\bar{x_{B}}=", "\\frac{", paste(x_3g2, collapse = " + "), "}{", n_per, "}=", m2, "$$",
          "\n",
          "$$\\bar{x_{C}}=", "\\frac{", paste(x_3g3, collapse = " + "), "}{", n_per, "}=", m3, "$$",
          "\n",
          "$$\\bar{G}= \\frac{\\sum\\limits _{i=1}^{p}\\sum\\limits _{m=1}^{n}x_{mi}}{N} = \\frac{", paste(all_3, collapse = " + "), "}{", length(all_3), "} =", grand_mean, "$$",
          "\n",
          "$$ s_{A}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_3g1, "-", m1, ")^2"), collapse = " + "), "}{", n_per, "-1}=", v1, "$$",
          "\n",
          "$$ s_{B}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_3g2, "-", m2, ")^2"), collapse = " + "), "}{", n_per, "-1}=", v2, "$$",
          "\n",
          "$$ s_{C}^2=\\dfrac{\\displaystyle", paste(paste0("(", x_3g3, "-", m3, ")^2"), collapse = " + "), "}{", n_per, "-1}=", v3, "$$",
          "\n",
          "$$ \\hat{\\sigma}_{Res}^2 = \\frac{s_{A}^2 + s_{B}^2 + s_{C}^2}{3} = \\frac{", v1, "+", v2, "+", v3, "}{3} =", res_var, "$$",
          "\n",
          "$$ \\hat{\\sigma}_{Sys}^2 = \\frac{n \\cdot \\sum\\limits_{i=1}^{p}(\\bar{A}_i-\\bar{G})^2}{p-1} = \\frac{", n_per, " \\cdot (", round((m1 - grand_mean)^2, 2), "+", round((m2 - grand_mean)^2, 2), "+", round((m3 - grand_mean)^2, 2), ")}{2} =", sys_var, "$$",
          "\n",
          "$$F_{emp} = \\frac{\\sigma^2_{Sys}}{\\sigma^2_{Res}} = \\frac{", sys_var, "}{", res_var, "} =", f_emp, "$$",
          "\n",
          "$$df_1 = 3-1=2 $$",
          "\n",
          "$$ df_2 = 3 \\cdot (", n_per, "-1) =", df2_anova, "$$",
          "$$ F_{krit(df_1=2, df_2=", df2_anova, ", \\alpha=0.95)} =", f_crit, "$$"
        )
      },

      "confint_mean_with_var" = paste(
        "L\u00f6sungsweg: $$\\mu_{1,2}=", round(mean(x), 2), "\\pm z_{1-\\frac{", 0.05, "}{2}}\\frac{\\sqrt{", round(var(x), 2), "}}{\\sqrt{", length(x), "}}", "$$",
        "\n",
        "$$\\mu_{1,2}=", round(mean(x), 2), "\\pm", 1.96, "\\frac{", round(sd(x), 2), "}{", round(sqrt(length(x)), 2), "}", "$$",
        "\n",
        "$$\\mu_{1,2}=", round(mean(x), 2), "\\pm", round(1.96 * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$",
        "\n",
        "$$\\mu_{1}=", round(mean(x), 2) - round(1.96 * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$",
        "\n",
        "$$\\mu_{2}=", round(mean(x), 2) + round(1.96 * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$"
      ),

      "confint_mean_no_var" = paste(
        "L\u00f6sungsweg: $$df=", length(x), "-1=", length(x) - 1, "$$",
        "\n",
        "$$\\mu_{1,2}=", round(mean(x), 2), "\\pm t_{1-\\frac{", 0.05, "}{2}}\\frac{\\sqrt{", round(var(x), 2), "}}{\\sqrt{", length(x), "}}", "$$",
        "\n",
        "$$\\mu_{1,2}=", round(mean(x), 2), "\\pm", round(qt(0.975, length(x) - 1), 3), "\\frac{", round(sd(x), 2), "}{", round(sqrt(length(x)), 2), "}", "$$",
        "\n",
        "$$\\mu_{1,2}=", round(mean(x), 2), "\\pm", round(round(qt(0.975, length(x) - 1), 3) * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$",
        "\n",
        "$$\\mu_{1}=", round(mean(x), 2) - round(round(qt(0.975, length(x) - 1), 3) * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$",
        "\n",
        "$$\\mu_{2}=", round(mean(x), 2) + round(round(qt(0.975, length(x) - 1), 3) * round(sd(x), 2) / round(sqrt(length(x)), 2), 2), "$$"
      ),

      NULL
    )
  })

  plot_reactive <- eventReactive(input$solve, {
    d    <- sim_data()
    x    <- d$x; y <- d$y; n <- d$n
    x_g1 <- d$x_g1; x_g2 <- d$x_g2
    x_3g1 <- d$x_3g1; x_3g2 <- d$x_3g2; x_3g3 <- d$x_3g3
    df_t <- d$df_t; diff <- d$diff
    plot_df <- data.frame(ID = factor(1:n), X = x)

    switch(input$drop,
      "modalwert" = {
        x_mod   <- as.numeric(names(which.max(table(x))))
        freq_df <- as.data.frame(table(X = x))
        freq_df$X     <- as.numeric(as.character(freq_df$X))
        freq_df$modal <- freq_df$X == x_mod
        ggplot(freq_df, aes(x = factor(X), y = Freq, fill = modal)) +
          geom_bar(stat = "identity", colour = "black") +
          scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black")) +
          annotate("text", x = which(levels(factor(freq_df$X)) == as.character(x_mod)),
                   y = max(freq_df$Freq) + 0.3,
                   label = paste0("x[mod]==", x_mod), parse = TRUE,
                   size = 5, color = "black", fontface = "bold") +
          labs(x = "X", y = "H\u00e4ufigkeit") +
          theme_classic(base_size = 14) +
          theme(legend.position = "none")
      },

      "mean" = ggplot(plot_df, aes(x = ID, y = X)) +
        geom_point() + labs(x = "ID", y = "X") +
        geom_hline(yintercept = mean(x), linetype = "dashed") +
        annotate("label", x = n/2, y = mean(x), label = "Mittelwert") +
        theme_classic(base_size = 14),

      "sd" = ggplot(plot_df, aes(x = ID, y = X)) +
        geom_point() + labs(x = "ID", y = "X") +
        geom_hline(yintercept = mean(x),          linetype = "dashed") +
        geom_hline(yintercept = mean(x) + sd(x),  linetype = "dashed") +
        geom_hline(yintercept = mean(x) - sd(x),  linetype = "dashed") +
        annotate("label", x = n/2, y = mean(x),         label = "Mittelwert") +
        annotate("label", x = n/2, y = mean(x) + sd(x), label = "Mittelwert + SD") +
        annotate("label", x = n/2, y = mean(x) - sd(x), label = "Mittelwert - SD") +
        theme_classic(base_size = 14),

      "median" = ggplot(plot_df, aes(x = ID, y = X)) +
        geom_point() + labs(x = "ID", y = "X") +
        geom_hline(yintercept = median(x), linetype = "dashed") +
        annotate("label", x = n/2, y = median(x), label = "Median") +
        theme_classic(base_size = 14),

      "spannweite" = {
        y_pad <- (max(x) - min(x)) * 0.2
        ggplot(plot_df, aes(x = ID, y = X)) +
          geom_point() + labs(x = "ID", y = "X") +
          geom_hline(yintercept = min(x), linetype = "dashed") +
          geom_hline(yintercept = max(x), linetype = "dashed") +
          annotate("text", x = n/2, y = min(x), label = paste0("x[min]==", min(x)),
                   parse = TRUE, vjust = 1.5, size = 5, color = "black") +
          annotate("text", x = n/2, y = max(x), label = paste0("x[max]==", max(x)),
                   parse = TRUE, vjust = -0.5, size = 5, color = "black") +
          scale_y_continuous(limits = c(min(x) - y_pad, max(x) + y_pad)) +
          coord_cartesian(clip = "off") +
          theme_classic(base_size = 14)
      },

      "boxplot" = {
        xs <- sort(x)
        md <- if (n %% 2 == 0) (xs[n/2] + xs[n/2+1]) / 2 else xs[(n+1)/2]
        if ((n * 0.25) %% 1 == 0) {
          q1 <- (xs[n*0.25] + xs[n*0.25+1]) / 2
          q3 <- (xs[n*0.75] + xs[n*0.75+1]) / 2
        } else {
          q1 <- xs[ceiling(n*0.25)]
          q3 <- xs[ceiling(n*0.75)]
        }
        iqr_val  <- q3 - q1
        w_low    <- min(xs[xs >= q1 - 1.5 * iqr_val])
        w_high   <- max(xs[xs <= q3 + 1.5 * iqr_val])
        outliers <- xs[xs < q1 - 1.5 * iqr_val | xs > q3 + 1.5 * iqr_val]
        bp_df <- data.frame(grp = "X", ymin = w_low, lower = q1,
                            middle = md, upper = q3, ymax = w_high)
        p <- ggplot(bp_df, aes(x = grp, ymin = ymin, lower = lower,
                               middle = middle, upper = upper, ymax = ymax)) +
          geom_boxplot(stat = "identity", width = 0.4, fill = "white", colour = "black") +
          labs(x = "", y = "X") +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"))
        if (length(outliers) > 0) {
          p <- p + geom_point(data = data.frame(grp = "X", y = outliers),
                              aes(x = grp, y = y), inherit.aes = FALSE,
                              shape = 1, size = 3, colour = "black")
        }
        p
      },

      "cov" = ,
      "cor_pearson" = ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, colour = "black", fullrange = TRUE) +
        scale_x_continuous(limits = c(0, max(x) * 1.1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, max(y) * 1.1), expand = c(0, 0)) +
        labs(x = "X", y = "Y") +
        theme_classic(base_size = 14),

      "cor_sig" = ,
      "cor_sig2" = ,
      "cor_sig_s" = ,
      "cor_sig_s2" = {
        two_sided <- input$drop %in% c("cor_sig", "cor_sig_s")
        r_val <- if (input$drop %in% c("cor_sig", "cor_sig2")) round(cor(x, y), 2) else {
                   xr <- d$x_rank; yr <- d$y_rank
                   round(1 - 6 * sum((xr - yr)^2) / (n * (n^2 - 1)), 2)
                 }
        df_val <- n - 2
        t_emp  <- round(r_val * sqrt(df_val) / sqrt(1 - r_val^2), 2)
        t_krit <- round(qt(if (two_sided) 0.975 else 0.95, df_val), 3)
        x_lim  <- max(4.5, abs(t_emp) + 2)

        scatter_plot <- ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, colour = "black", fullrange = TRUE) +
          scale_x_continuous(limits = c(0, max(x) * 1.1), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, max(y) * 1.1), expand = c(0, 0)) +
          labs(x = "X", y = "Y") +
          theme_classic(base_size = 14)

        t_plot <- ggplot(data.frame(t = c(-x_lim, x_lim)), aes(x = t))
        if (two_sided) {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(-x_lim, -t_krit), fill = "#e8e8e8") +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        } else {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        }
        x_breaks <- if (two_sided) c(-t_krit, 0, t_krit) else c(0, t_krit)
        t_plot <- t_plot +
          stat_function(fun = dt, args = list(df = df_val), colour = "black") +
          geom_segment(aes(x = t_emp, xend = t_emp, y = 0, yend = dt(t_emp, df_val)),
                       linetype = "dashed", color = "black") +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "t", y = "") +
          ggtitle(parse(text = paste0("'t-Verteilung (df = ", df_val, "),  ' * t[emp] * ' = ", t_emp, "'"))) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9),
                plot.title = element_text(size = 11))

        cowplot::plot_grid(scatter_plot, t_plot, ncol = 2)
      },

      "lm" = {
        a_val <- round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2)
        b_val <- round(cov(x, y) / var(x), 2)
        x_max <- max(x) * 1.1
        y_max <- max(max(y), a_val + b_val * x_max) * 1.1
        ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
          geom_point() +
          geom_abline(intercept = a_val, slope = b_val, colour = "black") +
          geom_point(aes(x = 0, y = a_val), shape = 16, size = 3, colour = "black") +
          annotate("text", x = 0, y = a_val,
                   label = paste0("a == ", a_val), parse = TRUE,
                   hjust = -0.2, vjust = -0.5, size = 5, colour = "black") +
          scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
          labs(x = "X", y = "Y") +
          theme_classic(base_size = 14)
      },

      "lm_sig" = ,
      "lm_sig2" = {
        two_sided <- input$drop == "lm_sig"
        b_val     <- round(cov(x, y) / var(x), 2)
        sx        <- round(sqrt(var(x)), 2)
        sy        <- round(sqrt(var(y)), 2)
        df_val    <- n - 2
        numer_sig <- round(n * sy^2 - n * b_val^2 * sx^2, 3)
        sigma_hat <- round(sqrt(numer_sig / df_val), 3)
        s_b       <- round(sigma_hat / (sx * sqrt(n)), 3)
        t_emp     <- round(b_val / s_b, 2)
        t_krit    <- if (two_sided) round(qt(0.975, df_val), 3) else round(qt(0.95, df_val), 3)
        x_lim     <- max(4.5, abs(t_emp) + 2)

        a_val <- round(mean(y) - b_val * mean(x), 2)
        x_max <- max(x) * 1.1
        y_max <- max(max(y), a_val + b_val * x_max) * 1.1
        scatter_plot <- ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
          geom_point() +
          geom_abline(intercept = a_val, slope = b_val, colour = "black") +
          scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
          labs(x = "X", y = "Y") +
          theme_classic(base_size = 14)

        t_plot <- ggplot(data.frame(t = c(-x_lim, x_lim)), aes(x = t))
        if (two_sided) {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(-x_lim, -t_krit), fill = "#e8e8e8") +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        } else {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        }
        label_hjust <- if (t_emp >= 0) -0.15 else 1.15
        x_breaks    <- if (two_sided) c(-t_krit, 0, t_krit) else c(0, t_krit)
        t_plot <- t_plot +
          stat_function(fun = dt, args = list(df = df_val), colour = "black") +
          geom_segment(aes(x = t_emp, xend = t_emp, y = 0, yend = dt(t_emp, df_val)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = t_emp + 0.3 * sign(t_emp + 0.001), y = dt(0, df_val) * 0.85,
                   label = paste0("t[emp]~'='~", t_emp), parse = TRUE,
                   hjust = label_hjust, size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "t", y = "") +
          ggtitle(paste0("t-Verteilung (df = ", df_val, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(scatter_plot, t_plot, ncol = 2)
      },

      "lm_std" = {
        a_val <- round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2)
        b_val <- round(cov(x, y) / var(x), 2)
        x_max <- max(x) * 1.1
        y_max <- max(max(y), a_val + b_val * x_max) * 1.1
        ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
          geom_point() +
          geom_abline(intercept = a_val, slope = b_val, colour = "black") +
          scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
          labs(x = "X", y = "Y") +
          theme_classic(base_size = 14)
      },

      "lm_predict" = {
        a_val  <- round(mean(y) - (cov(x, y) / var(x)) * mean(x), 2)
        b_val  <- round(cov(x, y) / var(x), 2)
        x_pred <- d$x_pred
        y_hat  <- round(a_val + b_val * x_pred, 2)
        x_max  <- max(max(x), x_pred) * 1.1
        y_max  <- max(max(y), y_hat, a_val + b_val * x_max) * 1.1
        ggplot(data = d$df_2_plot, aes(x = X, y = Y)) +
          geom_point(colour = "grey60") +
          geom_abline(intercept = a_val, slope = b_val, colour = "black") +
          geom_segment(aes(x = x_pred, xend = x_pred, y = 0, yend = y_hat),
                       linetype = "dashed", colour = "#e74c3c") +
          geom_segment(aes(x = 0, xend = x_pred, y = y_hat, yend = y_hat),
                       linetype = "dashed", colour = "#e74c3c") +
          geom_point(aes(x = x_pred, y = y_hat), shape = 16, size = 4, colour = "#e74c3c") +
          annotate("text", x = x_pred, y = y_hat,
                   label = paste0("hat(y) == ", y_hat), parse = TRUE,
                   hjust = -0.2, vjust = -0.5, size = 5, colour = "#e74c3c") +
          scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
          labs(x = "X", y = "Y") +
          theme_classic(base_size = 14)
      },

      "t.test1" = ,
      "t.test1_2" = {
        two_sided <- input$drop == "t.test1"
        n_val  <- length(x)
        xbar   <- round(mean(x), 2)
        s_val  <- round(sqrt(var(x)), 2)
        se_val <- round(s_val / sqrt(n_val), 2)
        df_val <- n_val - 1
        t_emp  <- round((xbar - d$mu0) / se_val, 2)
        t_krit <- if (two_sided) round(qt(0.975, df_val), 3) else round(qt(0.95, df_val), 3)
        x_lim  <- max(4.5, abs(t_emp) + 2)

        y_max_bar <- max(xbar, d$mu0) * 1.15
        bar_plot <- ggplot(data.frame(Gruppe = "X", Mittelwert = xbar),
                           aes(x = Gruppe, y = Mittelwert)) +
          geom_bar(stat = "identity", fill = "white", colour = "black") +
          geom_hline(yintercept = d$mu0, linetype = "dashed", colour = "black") +
          annotate("text", x = 1.5, y = d$mu0,
                   label = paste0("mu[0] == ", d$mu0), parse = TRUE,
                   hjust = 0, vjust = -0.4, size = 5) +
          annotate("text", x = 1, y = xbar,
                   label = paste0("x\u0304 = ", xbar),
                   hjust = 0.5, vjust = -0.5, size = 5) +
          coord_cartesian(ylim = c(0, y_max_bar)) +
          labs(x = "", y = "X") +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"))

        t_plot <- ggplot(data.frame(t = c(-x_lim, x_lim)), aes(x = t))
        if (two_sided) {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(-x_lim, -t_krit), fill = "#e8e8e8") +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        } else {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        }
        label_hjust <- if (t_emp >= 0) -0.15 else 1.15
        x_breaks    <- if (two_sided) c(-t_krit, 0, t_krit) else c(0, t_krit)

        t_plot <- t_plot +
          stat_function(fun = dt, args = list(df = df_val), colour = "black") +
          geom_segment(aes(x = t_emp, xend = t_emp, y = 0, yend = dt(t_emp, df_val)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = t_emp + 0.3 * sign(t_emp + 0.001), y = dt(0, df_val) * 0.85,
                   label = paste0("t[emp]~'='~", t_emp), parse = TRUE,
                   hjust = label_hjust, size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "t", y = "") +
          ggtitle(paste0("t-Verteilung (df = ", df_val, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(bar_plot, t_plot, ncol = 2)
      },

      "abh_t.test" = ,
      "abh_t.test2" = {
        two_sided <- input$drop == "abh_t.test"
        df_dep <- nrow(df_t) - 1
        t_emp  <- round(round(mean(diff), 2) / round(round(sqrt(var(diff)), 2) / round(sqrt(nrow(df_t)), 2), 2), 2)
        t_krit <- if (two_sided) round(qt(0.975, df_dep), 3) else round(qt(0.95, df_dep), 3)
        x_lim  <- max(4.5, abs(t_emp) + 2)

        long_df <- rbind(
          data.frame(ID = factor(df_t$ID), Zeit = "t0", X = df_t$t0),
          data.frame(ID = factor(df_t$ID), Zeit = "t1", X = df_t$t1)
        )
        long_df$Zeit <- factor(long_df$Zeit, levels = c("t0", "t1"))

        mean_df <- data.frame(
          Zeit = factor(c("t0", "t1"), levels = c("t0", "t1")),
          X    = c(mean(df_t$t0), mean(df_t$t1))
        )
        y_max <- max(long_df$X) * 1.1

        line_plot <- ggplot(long_df, aes(x = Zeit, y = X, group = ID)) +
          geom_line(colour = "gray70") +
          geom_point(size = 2, colour = "gray70") +
          geom_line(data = mean_df, aes(group = 1), colour = "black", linewidth = 1.2) +
          geom_point(data = mean_df, aes(group = 1), colour = "black", size = 3) +
          scale_x_discrete(labels = c("t0" = expression(t[0]), "t1" = expression(t[1]))) +
          scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
          labs(x = "", y = "X") +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"))

        t_plot <- ggplot(data.frame(t = c(-x_lim, x_lim)), aes(x = t))
        if (two_sided) {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_dep), geom = "area",
                          xlim = c(-x_lim, -t_krit), fill = "#e8e8e8") +
            stat_function(fun = dt, args = list(df = df_dep), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        } else {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_dep), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        }
        label_hjust <- if (t_emp >= 0) -0.15 else 1.15
        x_breaks    <- if (two_sided) c(-t_krit, 0, t_krit) else c(0, t_krit)

        t_plot <- t_plot +
          stat_function(fun = dt, args = list(df = df_dep), colour = "black") +
          geom_segment(aes(x = t_emp, xend = t_emp, y = 0, yend = dt(t_emp, df_dep)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = t_emp + 0.3 * sign(t_emp + 0.001), y = dt(0, df_dep) * 0.85,
                   label = paste0("t[emp]~'='~", t_emp), parse = TRUE,
                   hjust = label_hjust, size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "t", y = "") +
          ggtitle(paste0("t-Verteilung (df = ", df_dep, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(line_plot, t_plot, ncol = 2)
      },

      "t.test" = ,
      "t.test2" = {
        two_sided <- input$drop == "t.test"
        df_val <- length(x_g1) + length(x_g2) - 2
        t_emp  <- unname(round(t.test(x_g1, x_g2)$statistic, 2))
        t_krit <- if (two_sided) round(qt(0.975, df_val), 3) else round(qt(0.95, df_val), 3)
        x_lim  <- max(4.5, abs(t_emp) + 2)

        bar_plot <- ggplot(
          data = rbind(data.frame(ID = as.character(1:(n/2)), X = x_g1, Gruppe = "A"),
                       data.frame(ID = as.character(1:(n/2)), X = x_g2, Gruppe = "B")),
          aes(x = Gruppe, y = X, fill = Gruppe)) +
          stat_summary(fun = "mean", geom = "bar", colour = "black") +
          stat_summary(aes(label = paste("x\u0304 =", round(..y.., 2))), fun = "mean", geom = "text", size = 5, vjust = -0.5) +
          scale_fill_manual(values = c("A" = "white", "B" = "black")) +
          labs(x = "Gruppe", y = "X") +
          coord_cartesian(ylim = c(0, max(c(mean(x_g1), mean(x_g2))) + max(c(mean(x_g1) * .1, mean(x_g2) * .1)))) +
          theme_classic(base_size = 14) +
          theme(legend.position = "none",
                text = element_text(color = "black"))

        t_plot <- ggplot(data.frame(t = c(-x_lim, x_lim)), aes(x = t))
        if (two_sided) {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(-x_lim, -t_krit), fill = "#e8e8e8") +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        } else {
          t_plot <- t_plot +
            stat_function(fun = dt, args = list(df = df_val), geom = "area",
                          xlim = c(t_krit, x_lim), fill = "#e8e8e8")
        }
        label_hjust <- if (t_emp >= 0) -0.15 else 1.15
        x_breaks <- if (two_sided) c(-t_krit, 0, t_krit) else c(0, t_krit)

        t_plot <- t_plot +
          stat_function(fun = dt, args = list(df = df_val), colour = "black") +
          geom_segment(aes(x = t_emp, xend = t_emp, y = 0, yend = dt(t_emp, df_val)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = t_emp + 0.3 * sign(t_emp + 0.001), y = dt(0, df_val) * 0.85,
                   label = paste0("t[emp]~'='~", t_emp),
                   parse = TRUE,
                   hjust = label_hjust, size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "t", y = "") +
          ggtitle(paste0("t-Verteilung (df = ", df_val, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(bar_plot, t_plot, ncol = 2)
      },

      "rm_anova" = {
        rm <- d$df_rm; n_s <- d$n_rm; k <- 3
        t0 <- rm$t0; t1 <- rm$t1; t2 <- rm$t2
        mt0 <- round(mean(t0), 2); mt1 <- round(mean(t1), 2); mt2 <- round(mean(t2), 2)
        gm  <- round(mean(c(t0, t1, t2)), 2)
        person_means <- round(rowMeans(rm[, c("t0","t1","t2")]), 2)
        ss_time  <- round(n_s * ((mt0-gm)^2 + (mt1-gm)^2 + (mt2-gm)^2), 3)
        ss_pers  <- round(k * sum((person_means - gm)^2), 3)
        ss_tot   <- round(sum((c(t0,t1,t2) - gm)^2), 3)
        ss_err   <- round(ss_tot - ss_time - ss_pers, 3)
        df_time  <- k - 1; df_err <- (k-1)*(n_s-1)
        f_emp    <- round((ss_time/df_time) / (ss_err/df_err), 2)
        f_crit   <- round(qf(0.95, df_time, df_err), 2)
        x_lim    <- max(8, f_emp + 2)

        long_df <- rbind(
          data.frame(ID = factor(rm$ID), Zeit = "t0", X = t0),
          data.frame(ID = factor(rm$ID), Zeit = "t1", X = t1),
          data.frame(ID = factor(rm$ID), Zeit = "t2", X = t2)
        )
        long_df$Zeit <- factor(long_df$Zeit, levels = c("t0","t1","t2"))
        mean_df <- data.frame(
          Zeit = factor(c("t0","t1","t2"), levels = c("t0","t1","t2")),
          X    = c(mt0, mt1, mt2)
        )
        y_max_rm <- max(long_df$X) * 1.1

        line_plot <- ggplot(long_df, aes(x = Zeit, y = X, group = ID)) +
          geom_line(colour = "gray70") +
          geom_point(size = 2, colour = "gray70") +
          geom_line(data = mean_df, aes(group = 1), colour = "black", linewidth = 1.2) +
          geom_point(data = mean_df, aes(group = 1), colour = "black", size = 3) +
          scale_x_discrete(labels = c("t0" = expression(t[0]),
                                      "t1" = expression(t[1]),
                                      "t2" = expression(t[2]))) +
          scale_y_continuous(limits = c(0, y_max_rm), expand = c(0, 0)) +
          labs(x = "", y = "X") +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"))

        label_hjust_rm <- if (f_emp < x_lim * 0.7) -0.15 else 1.15
        label_x_rm     <- f_emp + 0.2 * sign(f_emp - x_lim * 0.5 + 0.001)

        f_plot <- ggplot(data.frame(f = c(0, x_lim)), aes(x = f)) +
          stat_function(fun = df, args = list(df1 = df_time, df2 = df_err), geom = "area",
                        xlim = c(f_crit, x_lim), fill = "#e8e8e8") +
          stat_function(fun = df, args = list(df1 = df_time, df2 = df_err), colour = "black") +
          geom_segment(aes(x = f_emp, xend = f_emp, y = 0,
                           yend = df(f_emp, df_time, df_err)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = label_x_rm,
                   y = df(f_crit, df_time, df_err) * 1.8,
                   label = paste0("F[emp]~'='~", f_emp),
                   parse = TRUE, hjust = label_hjust_rm,
                   size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = c(0, f_crit)) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "F", y = "") +
          ggtitle(paste0("F-Verteilung (df1 = ", df_time, ", df2 = ", df_err, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(line_plot, f_plot, ncol = 2)
      },

      "oneway_anova" = {
        all_3      <- c(x_3g1, x_3g2, x_3g3)
        n_per      <- length(x_3g1)
        grand_mean <- round(mean(all_3), 2)
        m1 <- round(mean(x_3g1), 2); m2 <- round(mean(x_3g2), 2); m3 <- round(mean(x_3g3), 2)
        v1 <- round(var(x_3g1), 2);  v2 <- round(var(x_3g2), 2);  v3 <- round(var(x_3g3), 2)
        res_var    <- round((v1 + v2 + v3) / 3, 2)
        sys_var    <- round(n_per * round((m1 - grand_mean)^2 + (m2 - grand_mean)^2 + (m3 - grand_mean)^2, 2) / 2, 2)
        f_emp      <- round(sys_var / res_var, 2)
        df1_val    <- 2
        df2_val    <- 3 * (n_per - 1)
        f_crit     <- round(qf(0.95, df1_val, df2_val), 2)
        x_lim      <- max(8, f_emp + 2)

        bar_df <- rbind(
          data.frame(ID = as.character(1:n_per), X = x_3g1, Gruppe = "A"),
          data.frame(ID = as.character(1:n_per), X = x_3g2, Gruppe = "B"),
          data.frame(ID = as.character(1:n_per), X = x_3g3, Gruppe = "C")
        )
        bar_plot <- ggplot(bar_df, aes(x = Gruppe, y = X, fill = Gruppe)) +
          stat_summary(fun = "mean", geom = "bar", colour = "black") +
          stat_summary(aes(label = paste("x\u0304 =", round(..y.., 2))), fun = "mean",
                       geom = "text", size = 5, vjust = -0.5) +
          scale_fill_manual(values = c("A" = "white", "B" = "gray60", "C" = "black")) +
          labs(x = "Gruppe", y = "X") +
          coord_cartesian(ylim = c(0, max(m1, m2, m3) * 1.15)) +
          theme_classic(base_size = 14) +
          theme(legend.position = "none", text = element_text(color = "black"))

        label_hjust <- if (f_emp < x_lim * 0.7) -0.15 else 1.15
        label_x     <- f_emp + 0.2 * sign(f_emp - x_lim * 0.5 + 0.001)

        f_plot <- ggplot(data.frame(f = c(0, x_lim)), aes(x = f)) +
          stat_function(fun = df, args = list(df1 = df1_val, df2 = df2_val), geom = "area",
                        xlim = c(f_crit, x_lim), fill = "#e8e8e8") +
          stat_function(fun = df, args = list(df1 = df1_val, df2 = df2_val), colour = "black") +
          geom_segment(aes(x = f_emp, xend = f_emp, y = 0,
                           yend = df(f_emp, df1_val, df2_val)),
                       linetype = "dashed", color = "black") +
          annotate("text", x = label_x,
                   y = df(f_crit, df1_val, df2_val) * 1.8,
                   label = paste0("F[emp]~'='~", f_emp),
                   parse = TRUE, hjust = label_hjust,
                   size = 5, color = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = c(0, f_crit)) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "F", y = "") +
          ggtitle(paste0("F-Verteilung (df1 = ", df1_val, ", df2 = ", df2_val, "):")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(color = "black"),
                axis.text.x = element_text(color = "black", size = 9))

        cowplot::plot_grid(bar_plot, f_plot, ncol = 2)
      },

      "cohend" = ggplot(
        data = rbind(data.frame(ID = as.character(1:(n/2)), X = x_g1, Gruppe = "A"),
                     data.frame(ID = as.character(1:(n/2)), X = x_g2, Gruppe = "B")),
        aes(x = Gruppe, y = X, fill = Gruppe)) +
        stat_summary(fun = "mean", geom = "bar", colour = "black") +
        stat_summary(aes(label = paste("x\u0304 =", round(..y.., 2))), fun = "mean", geom = "text", size = 5, vjust = -0.5) +
        scale_fill_manual(values = c("A" = "white", "B" = "black")) +
        labs(x = "Gruppe", y = "Y") +
        coord_cartesian(ylim = c(0, max(c(mean(x_g1), mean(x_g2))) + max(c(mean(x_g1) * .1, mean(x_g2) * .1)))) +
        theme_classic(base_size = 14) + theme(legend.position = "none"),

      "zdist" = {
        pos   <- d$zdist_pos
        z_pos <- round(scale(x), 2)[pos]
        p_pos <- if (d$zdist_direction == "lower") round(pnorm(z_pos), 4) else round(1 - pnorm(z_pos), 4)
        shade_lim <- if (d$zdist_direction == "lower") c(-3.5, unname(z_pos)) else c(unname(z_pos), 3.5)
        ggplot(data.frame(z = c(-3.5, 3.5)), aes(x = z)) +
          stat_function(fun = dnorm, geom = "area",
                        xlim = shade_lim, fill = "#e8e8e8") +
          stat_function(fun = dnorm, colour = "black") +
          geom_segment(aes(x = unname(z_pos), xend = unname(z_pos),
                           y = 0, yend = dnorm(unname(z_pos))),
                       linetype = "dashed") +
          annotate("text", x = 2.5, y = 0.35,
                   label = paste0("z = ", z_pos, "\nP = ", p_pos),
                   hjust = 1, vjust = 1, size = 5, color = "black") +
          labs(x = "z", y = "") +
          scale_x_continuous(breaks = 0) +
          scale_y_continuous(breaks = NULL) +
          theme_classic(base_size = 14) +
          theme(axis.text   = element_text(colour = "black", size = 10),
                axis.title  = element_text(colour = "black"),
                plot.title  = element_text(colour = "black"))
      },

      "scale" = {
        mu  <- round(mean(x), 2)
        sig <- round(sd(x), 2)
        p_orig <- ggplot(data.frame(t = c(mu - 3.5 * sig, mu + 3.5 * sig)), aes(x = t)) +
          stat_function(fun = dnorm, args = list(mean = mu, sd = sig), colour = "black") +
          geom_segment(aes(x = mu, xend = mu, y = 0, yend = dnorm(mu, mu, sig)),
                       linetype = "dashed", colour = "black") +
          scale_x_continuous(breaks = c(mu - sig, mu, mu + sig),
                             labels = c(paste0("\u03bc-\u03c3\n(", mu - sig, ")"),
                                        paste0("\u03bc\n(", mu, ")"),
                                        paste0("\u03bc+\u03c3\n(", mu + sig, ")"))) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "", y = "", title = paste0("Originalverteilung: N(", mu, ", ", sig, ")")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(colour = "black"),
                axis.text.x = element_text(colour = "black", size = 11))

        p_std <- ggplot(data.frame(t = c(-3.5, 3.5)), aes(x = t)) +
          stat_function(fun = dnorm, args = list(mean = 0, sd = 1), colour = "black") +
          geom_segment(aes(x = 0, xend = 0, y = 0, yend = dnorm(0)),
                       linetype = "dashed", colour = "black") +
          scale_x_continuous(breaks = c(-1, 0, 1),
                             labels = c("\u03bc-\u03c3\n(-1)", "\u03bc\n(0)", "\u03bc+\u03c3\n(1)")) +
          scale_y_continuous(breaks = NULL) +
          labs(x = "", y = "", title = "Standardnormalverteilung: N(0, 1)") +
          theme_classic(base_size = 14) +
          theme(text = element_text(colour = "black"),
                axis.text.x = element_text(colour = "black", size = 11))

        cowplot::plot_grid(p_orig, p_std, ncol = 2)
      },

      "chi2_gleich" = ,
      "chi2_verh"   = ,
      "chi2_2d"     = ,
      "chi2_4feld"  = {
        # compute stats
        if (input$drop %in% c("chi2_gleich", "chi2_verh")) {
          if (input$drop == "chi2_gleich") {
            obs      <- d$obs_chi2_gleich; k <- d$k_chi2_gleich; n <- sum(obs)
            exp_vals <- rep(n / k, k); cats <- paste0("K", 1:k)
          } else {
            obs      <- d$obs_chi2_verh; probs <- d$probs_verh; n <- sum(obs)
            exp_vals <- n * probs; k <- length(obs); cats <- paste0("K", 1:k)
          }
          chi2_emp  <- round(sum((obs - exp_vals)^2 / exp_vals), 3)
          df_chi2   <- k - 1
          chi2_krit <- round(qchisq(0.95, df_chi2), 3)
          bar_df <- data.frame(
            Kategorie   = factor(rep(cats, 2), levels = cats),
            Typ         = rep(c("Beobachtet", "Erwartet"), each = k),
            Haeufigkeit = c(obs, round(exp_vals, 1))
          )
          bar_plot <- ggplot(bar_df, aes(x = Kategorie, y = Haeufigkeit, fill = Typ)) +
            geom_bar(stat = "identity", position = "dodge", colour = "black") +
            scale_fill_manual(values = c("Beobachtet" = "black", "Erwartet" = "white")) +
            labs(x = "Kategorie", y = "H\u00e4ufigkeit", fill = "") +
            theme_classic(base_size = 14) +
            theme(text = element_text(color = "black"), legend.position = "bottom")
        } else {
          obs <- if (input$drop == "chi2_2d") d$obs_chi2_2d else d$obs_chi2_4feld
          row_tots <- rowSums(obs); col_tots <- colSums(obs); n <- sum(obs)
          exp_mat   <- outer(row_tots, col_tots) / n
          chi2_emp  <- round(sum((obs - exp_mat)^2 / exp_mat), 3)
          df_chi2   <- if (input$drop == "chi2_4feld") 1L else (nrow(obs) - 1L) * (ncol(obs) - 1L)
          chi2_krit <- round(qchisq(0.95, df_chi2), 3)
          bar_df <- data.frame(
            Kategorie   = factor(rep(colnames(obs), each = nrow(obs)), levels = colnames(obs)),
            Gruppe      = factor(rep(rownames(obs), ncol(obs))),
            Haeufigkeit = as.vector(obs)
          )
          bar_plot <- ggplot(bar_df, aes(x = Kategorie, y = Haeufigkeit, fill = Gruppe)) +
            geom_bar(stat = "identity", position = "dodge", colour = "black") +
            scale_fill_manual(values = c("A" = "white", "B" = "black")) +
            labs(x = "Kategorie", y = "H\u00e4ufigkeit", fill = "Gruppe") +
            theme_classic(base_size = 14) +
            theme(text = element_text(color = "black"), legend.position = "bottom")
        }

        x_lim      <- max(12, chi2_emp + 3)
        peak_x     <- max(0.01, df_chi2 - 1)
        label_hjust <- if (chi2_emp < x_lim * 0.65) -0.15 else 1.15
        label_x     <- chi2_emp + 0.3 * sign(chi2_emp - x_lim * 0.5 + 0.001)

        chi2_plot <- ggplot(data.frame(x = c(0, x_lim)), aes(x = x)) +
          stat_function(fun = dchisq, args = list(df = df_chi2), geom = "area",
                        xlim = c(chi2_krit, x_lim), fill = "#e8e8e8") +
          stat_function(fun = dchisq, args = list(df = df_chi2), colour = "black") +
          geom_segment(aes(x = chi2_emp, xend = chi2_emp,
                           y = 0, yend = dchisq(chi2_emp, df_chi2)),
                       linetype = "dashed", colour = "black") +
          annotate("text", x = label_x, y = dchisq(peak_x, df_chi2) * 0.7,
                   label = paste0("chi[emp]^2~'='~", chi2_emp),
                   parse = TRUE, hjust = label_hjust,
                   size = 5, colour = "black", fontface = "bold") +
          coord_cartesian(clip = "off") +
          scale_x_continuous(breaks = c(0, chi2_krit)) +
          scale_y_continuous(breaks = NULL) +
          labs(x = expression(chi^2), y = "") +
          ggtitle(bquote(chi^2 ~ "-Verteilung (df =" ~ .(df_chi2) ~ ")")) +
          theme_classic(base_size = 14) +
          theme(text = element_text(colour = "black"),
                axis.text.x = element_text(colour = "black", size = 9))

        cowplot::plot_grid(bar_plot, chi2_plot, ncol = 2)
      },

      NULL
    )
  })

  # Button visibility -------------------------------------------------------

  observeEvent(input$go, {
    output$solve_button <- renderUI({ actionButton("solve", "L\u00f6sung") })
    if (input$drop %in% c("t.test1", "t.test1_2", "abh_t.test", "abh_t.test2", "t.test", "t.test2", "zdist", "oneway_anova", "rm_anova",
                          "chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld", "lm_sig", "lm_sig2")) {
      output$table_button <- renderUI({ actionButton("tab", "Verteilungstabelle") })
    } else {
      output$table_button <- renderUI({ NULL })
    }
  })

  observeEvent(input$solve, {
    output$reset_button <- renderUI({ actionButton("reset", "Reset") })
  })

  observeEvent(input$reset, { session$reload() })

  # Outputs -----------------------------------------------------------------

  output$text  <- renderUI({ withMathJax(helpText(HTML(text_reactive()))) })
  output$text2 <- renderUI({ withMathJax(helpText(text2_reactive())) })

  output$table <- renderTable({
    table_reactive()
  }, rownames = TRUE, colnames = FALSE, bordered = TRUE, caption = "<strong>Daten:</strong>", caption.placement = "top")

  output$table_alt <- renderTable({
    table_alt_reactive()
  }, rownames = FALSE, colnames = TRUE, bordered = TRUE, sanitize.colnames.function = identity)

  observeEvent(input$tab, {
    caption_text <- switch(input$drop,
      "zdist"        = "Verteilungsfunktion der Standardnormalverteilung:",
      "oneway_anova" = "Verteilungsfunktion der F-Verteilungen:",
      "chi2_gleich"  = ,
      "chi2_verh"    = ,
      "chi2_2d"      = ,
      "chi2_4feld"   = "Verteilungsfunktion der \u03c7\u00b2-Verteilung:",
      "Verteilungsfunktion der t-Verteilungen:"
    )
    digits_val <- if (input$drop == "zdist") 4 else
                  if (input$drop == "oneway_anova") 2 else
                  if (input$drop %in% c("chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld")) 3 else 0
    output$table2 <- renderTable({
      table2_reactive()
    }, rownames = FALSE, colnames = TRUE, bordered = TRUE,
       digits = digits_val, caption = caption_text, caption.placement = "top")
  })

  output$table3        <- renderTable({ table3_reactive() },        rownames = FALSE, colnames = TRUE, bordered = TRUE, sanitize.colnames.function = identity)
  output$loesung_table_label <- renderUI({
    req(input$solve)
    if (input$drop == "xtable") helpText(HTML("<strong>L\u00f6sung:</strong>")) else NULL
  })
  output$loesung_table <- renderTable({ loesung_table_reactive() }, rownames = TRUE,  colnames = TRUE, bordered = TRUE, digits = 0)
  output$formel        <- renderUI({ withMathJax(helpText(HTML(sub("^Formel:", "<strong>Formel:</strong>", formel_reactive())))) })
  output$loesungsweg   <- renderUI({ withMathJax(helpText(HTML(sub("^L\u00f6sungs?weg:", "<strong>L\u00f6sungsweg:</strong>", loesungsweg_reactive())))) })
  output$loesung       <- renderUI({ withMathJax(helpText(HTML(sub("^L\u00f6sung:", "<strong>L\u00f6sung:</strong>", loesung_reactive())))) })
  plots_with_label <- c("modalwert", "mean", "sd", "median", "spannweite", "boxplot", "cov", "cor_pearson", "cor_sig", "cor_sig2", "cor_sig_s", "cor_sig_s2", "lm", "lm_predict", "lm_std", "lm_sig", "lm_sig2",
                        "t.test1", "t.test1_2", "abh_t.test", "abh_t.test2", "t.test", "t.test2", "cohend", "oneway_anova", "rm_anova",
                        "chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld", "zdist", "scale")
  output$plot_label <- renderUI({
    req(input$solve)
    if (input$drop %in% plots_with_label) {
      helpText(HTML("<strong>Graphische Darstellung:</strong>"))
    } else {
      NULL
    }
  })

  output$plot_container <- renderUI({
    if (input$drop %in% c("t.test1", "t.test1_2", "abh_t.test", "abh_t.test2", "t.test", "t.test2", "oneway_anova", "rm_anova", "scale",
                          "chi2_gleich", "chi2_verh", "chi2_2d", "chi2_4feld", "lm_sig", "lm_sig2", "cor_sig", "cor_sig2", "cor_sig_s", "cor_sig_s2")) {
      plotOutput("plot", width = "580px", height = "280px")
    } else {
      plotOutput("plot", width = "300px", height = "300px")
    }
  })
  output$plot          <- renderPlot({ plot_reactive() })
}

# Run the application

shinyApp(ui = ui, server = server)
