using System;
using System.Runtime.CompilerServices;

namespace OilGas.Data
{
    public sealed class OilPvtProperties
    {

    }

    public static class Pvt
    {
        // f (GasSolubility[i] < GOR[i])
        // FreeGasFlowRate[i]=GasFlowRate[i]−(GasSolubility[i]*OilFlowRate[i])
        // else FreeGasFlowRate[i]=0 

        public static class Constants
        {
            #region Constants

            public const double QGSFACT = 1000.0;

            /// For calculations that require iteration, some default values
            public const double DPGUESS = 100.0; // For pressure traverse, guessed delta P for segment (to help kickstart iterations)

            public const double CONVTOL = 1E-12; // Convergence tolerance
            public const int    MAXITER = 1000;  // maximum number of iterations

            /// Default number of points to use for inflow/outflow curves
            public const int DEFNUMIOPTS = 30;

            /// Default grid spacing for wellbore gradient computations
            public const double DEFWBDX = 100.0;

            /// <summary>
            /// Typedef for a Tuple of X-Y values (e.g. Q-P for inflow or outflow curve, etc)
            /// </summary>
            ///typedef Tuple<List<double>, List<double> > Curve;
            /// <summary>
            /// Typedef for a triplet of values where the second and third arguments are functions of the first argument.
            /// Most commonly, this is used for wellbore traverse, where P (2nd arg) and T (3rd arg) are functions of MD (first arg)
            /// </summary>
            //typedef tuple<List<double>, List<double>, List<double> > DoubleCurve;
            /// <summary>Typedef for collection of x-y pairs (useful for, e.g., sensitivity analysis)</summary>
            /// typedef List
            /// <Curve> CurveSet; Conversion factors
            public const double BBL_CF = 5.615; // Convert bbls to cubic feet

            public const double DAY_SEC   = 86400.0;      // Convert days to seconds
            public const double FT_IN     = 12.0;         // Convert feet to inches
            public const double FT2_IN2   = 144.0;        // Convert ft^2 to in^2
            public const double G_C       = 32.174049;    // lb ft/s^2/lbf
            public const double LBMFTS_CP = 1488.1639;    // Convert lbm/(ft s) to cp
            public const double DCM_LBFFT = 6.8521766e-5; // convert dynes/cm to lbf/ft
            public const double F_TO_R    = 459.67;        // add this to degF to convert to degR

            /// Standard condition values
            public const double TSC = 60.0 + F_TO_R; // degR

            public const double PSC = 14.7; // psia
            public const double ZSC = 1.0;  // Z-factor

            /// Constants
            public const double RGAS = 10.7315; // Universal gas constant, psi ft^3/lbmol degR

            public const double STDAIRMA    = 28.96;                               // Apparent molar mass of air
            public const double STDWATERRHO = 62.3664;                             // Density of water at 14.7 psia and 60 degF
            public const double LBFT3TOGCM3 = 62.42797;                            // Conversion from LB/FT3 to G/CM3
            public const double STDAIRRHO   = PSC * STDAIRMA / (ZSC * RGAS * TSC); // Density of air at standard atmospheric pressure and temperature
            public const double PI          = 3.14159265358979323846;              // pi

            public const double NLV_MULT = 1.9377; // Multiplier for liquid (gas) velocity number in order to use it with oil units

            public const double ND_MULT = 120.8052; // Multiplier for diameter number in order to use it with oil units

            public const double NL_MULT = 0.1573; // Multiplier for liquid viscosity number in order to use it with oil units

            /// <summary>Default seperator pressure and temperature values for all Vasquez-Beggs correlations.</summary>
            public const double PSEPVB_DEF = 114.7; // psia

            public const double TSEPVB_DEF = 520.0; // degR 

            #endregion
        }

        /// <summary>
        /// Bo
        /// </summary>
        public static class OilFormationVolumeFactor
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double VasquezBeggs(double Rs,
                                              double T,
                                              double SGo_API,
                                              double SGg)
            {
                // Correct gas specific gravity
                double SGg_gs = GasSolubility.correctGasSGVB(SGg, SGo_API, Constants.PSEPVB_DEF, Constants.TSEPVB_DEF);

                //Constants depend on value of API

                if(SGo_API <= 30)
                {
                    const double C1 = 4.677e-4;
                    const double C2 = 1.751e-5;
                    const double C3 = -1.811e-8;

                    return 1.0 + C1 * Rs + (T + Constants.F_TO_R - 60) * (SGo_API / SGg_gs) * (C2 + C3 * Rs);
                }
                else
                {
                    const double C1 = 4.67e-4;
                    const double C2 = 1.100e-5;
                    const double C3 = 1.337e-9;

                    return 1.0 + C1 * Rs + (T + Constants.F_TO_R - 60) * (SGo_API / SGg_gs) * (C2 + C3 * Rs);
                }
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Glaso(double Rs,
                                       double T,
                                       double SGo_API,
                                       double SGg)
            {
                // Compute SGo
                double SGo = API_to_SG(SGo_API);

                double Bost = Rs * Math.Pow(SGg / SGo, 0.526) + 0.968 * (T + Constants.F_TO_R);

                double A = -6.58511 + 2.91329 * Math.Log10(Bost) - 0.27683 * Math.Pow(Math.Log10(Bost), 2.0);

                return 1.0 + Math.Pow(10.0, A);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Petrosky(double Rs,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // Compute SGo
                double SGo = API_to_SG(SGo_API);

                double a = Math.Pow(Rs, 0.3738);
                double b = Math.Pow(SGg, 0.2914) / Math.Pow(SGo, 0.6265);
                double c = Math.Pow(T + Constants.F_TO_R, 0.5371);

                return 1.0113 + 7.2046e-5 * Math.Pow(a * b + 0.24626 * c, 3.0936);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Standing(double Rs,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // Compute SGo
                double SGo = API_to_SG(SGo_API);

                return 0.9759 + 0.000120 * Math.Pow(Rs * Math.Sqrt(SGg / SGo) + 1.25 * (T + Constants.F_TO_R), 1.2);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Marhoun(double Rs,
                                         double T,
                                         double SGo_API,
                                         double SGg)
            {
                const double a = 0.74239;
                const double b = 0.323294;
                const double c = -1.20204;
                double       f = (Math.Pow(Rs, a)) * (Math.Pow(SGg, b)) * (Math.Pow(SGo_API, c));

                return 0.497069 + 0.000862963 * T + 0.00182594 * f;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double MaterialBalance(double Rs,
                                                 double T,
                                                 double SGo_API,
                                                 double SGg)
            {
                double rho_o = SGo_API * 62.4;

                return (62.4 * SGo_API + 0.0136 * Rs * SGg) / rho_o;
            }
        }

        /// <summary>
        /// Bw
        /// </summary>
        public static class WaterFormationVolumeFactor
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double McCain(double P,
                                        double T,
                                        double S = 0.0)
            {
                // Convert temperature to degF (make the following easier
                T = T + Constants.F_TO_R;

                double dVt = -1.0001e-2 + 1.33391e-4 * T                                    + 5.50654e-7 * T  * T;
                double dVp = -1.95301e-9 * P * T - 1.72834e-13 * P * P * T - 3.58922e-7 * P - 2.25341e-10 * P * P;

                return (1.0 + dVt) * (1.0 + dVp);
            }
        }

        /// <summary>
        /// Bt
        /// </summary>
        public static class TotalFormationVolumeFactor
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Standing(double P,
                                          double T,
                                          double gamma_g,
                                          double API,
                                          double Rs,
                                          double flag)
            {
                // Convert temperature to degF (make the following easier
                T = T + Constants.F_TO_R;
                double gamma_o = 141.5 / (API + 131.5);

                // Standing Correlation
                double c = 2.9 * Math.Pow(10, (-0.00027 * Rs));

                double log_A_star = (Math.Log(Rs                         * ((Math.Pow((T - 460), 0.5)) * (Math.Pow(gamma_o, c))) / Math.Pow(gamma_g, 0.3)) / 2.30258509299405) -
                                    (10.1 - 96.8 / (6.604 + (Math.Log(P) / 2.30258509299405)));

                double log_Bt = -5.223 - 47.4 / (-12.22 + log_A_star);

                return Math.Pow(10, (log_Bt));
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Glaso(double P,
                                       double T,
                                       double gamma_g,
                                       double API,
                                       double Rs,
                                       double flag)
            {
                // Convert temperature to degF (make the following easier
                T = T + Constants.F_TO_R;
                double gamma_o = 141.5 / (API + 131.5);
                // Glaso's Correlation
                double c          = 2.9                                                                                   * Math.Pow(10, (-0.00027 * Rs));
                double A_star     = ((Rs * (Math.Pow((T - 460), 0.5)) * (Math.Pow(gamma_o, c))) / Math.Pow(gamma_g, 0.3)) * Math.Pow(P,  (-1.1089));
                double log_A_star = Math.Log(A_star)                                                                      / Math.Log(10);
                double log_Bt     = 0.080135 + 0.47257 * log_A_star + 0.17351 * log_A_star * log_A_star;

                return Math.Pow(10, log_Bt);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Marhoun(double P,
                                         double T,
                                         double gamma_g,
                                         double API,
                                         double Rs,
                                         double flag)
            {
                // Convert temperature to degF (make the following easier
                T = T + Constants.F_TO_R;
                double gamma_o = 141.5 / (API + 131.5);

                // Marhoun's Correlation
                double a = 0.644516;
                double b = -1.07934;
                double c = 0.724874;
                double d = 2.00621;
                double e = -0.76191;
                double F = (Math.Pow(Rs, a)) * (Math.Pow(gamma_g, b)) * (Math.Pow(gamma_o, c)) * (Math.Pow(T, d)) * (Math.Pow(P, e));

                return 0.314693 + 0.0000106253 * F + 0.000000000018883 * F * F;
            }
        }

        /// <summary>
        /// Rs
        /// </summary>
        public static class GasSolubility
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Standing(double P,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double a = 0.00091 * T - 0.0125 * SGo_API;

                double b = Math.Pow(10.0, -a);
                //double Rs =  SGg*Math.Pow(b*(P/18.2 + 1.4),1./0.83);

                double Rs = SGg * Math.Pow(Math.Pow(10.0, 0.0125 * SGo_API - 0.00091 * T) * P / 18.0, 1 / 0.83); // NETool uses this, slight different...

                return Rs;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Glaso(double P,
                                       double T,
                                       double SGo_API,
                                       double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double x    = 2.8869 - Math.Sqrt(14.1811 - 3.3093 * Math.Log10(P));
                double pbst = Math.Pow(10.0, x);

                double b = Math.Pow(SGo_API, 0.989) / Math.Pow(T, 0.172);

                return SGg * Math.Pow(b * pbst, 1.2255);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Petrosky(double P,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double x = 4.561e-5 * Math.Pow(T, 1.3911) - 7.916e-4 * Math.Pow(SGo_API, 1.5410);

                double b = Math.Pow(10.0, -x);

                double c = Math.Pow(SGg, 0.8439);

                return Math.Pow((P / 112.727 + 12.34) * c * b, 1.73184);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double VasquezBeggs(double P,
                                              double T,
                                              double SGo_API,
                                              double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                // Correct gas specific gravity
                double SGg_gs = correctGasSGVB(SGg, SGo_API, Constants.PSEPVB_DEF, Constants.TSEPVB_DEF);

                if(SGo_API <= 30.0)
                {
                    const double C1 = 0.0362;
                    const double C2 = 1.0937;
                    const double C3 = 25.7240;

                    return C1 * SGg_gs * Math.Pow(P, C2) * Math.Exp(C3 * SGo_API / (T + Constants.F_TO_R));
                }
                else
                {
                    const double C1 = 0.017838;
                    const double C2 = 1.1870;
                    const double C3 = 23.93508;

                    return C1 * SGg_gs * Math.Pow(P, C2) * Math.Exp(C3 * SGo_API / (T + Constants.F_TO_R));
                }
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Marhoun(double P,
                                         double T,
                                         double SGo_API,
                                         double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double a       = 185.843208;
                double b       = 1.87784;
                double c       = -3.1437;
                double d       = -1.32657;
                double e       = 1.398441;
                double gamma_o = 141.5 / (SGo_API + 131.5);

                return Math.Pow((a * (Math.Pow(SGg, b)) * (Math.Pow(gamma_o, c)) * (Math.Pow(T, d)) * P), e);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double correctGasSGVB(double SGg,
                                                double SGo_API,
                                                double Psep,
                                                double Tsep)
            {
                // this correlation assumes Tsep is in degF
                Tsep = Tsep - Constants.F_TO_R;

                double a = 5.912e-5;

                return SGg * (1.0 + a * SGo_API * Tsep * Math.Log10(Psep / 114.7));
            }
        }

        public static class BubblePointPressure
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Standing(double GOR,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double a = 0.00091 * T - 0.0125 * SGo_API;
                double b = Math.Pow(10.0, a);

                return 18.2 * (Math.Pow(GOR / SGg, 0.83) * b - 1.4);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Petrosky(double GOR,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double x = 4.561e-5 * Math.Pow(T, 1.3911) - 7.916e-4 * Math.Pow(SGo_API, 1.5410);
                double b = Math.Pow(10.0, x);
                double c = Math.Pow(GOR, 0.5774) / Math.Pow(SGg, 0.8439);

                return 112.727 * (c * b - 12.340);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double VasquezBeggs(double GOR,
                                              double T,
                                              double SGo_API,
                                              double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                // Correct gas specific gravity
                double SGg_gs = GasSolubility.correctGasSGVB(SGg, SGo_API, Constants.PSEPVB_DEF, Constants.TSEPVB_DEF);

                // Constants depend on value of API
                if(SGo_API <= 30)
                {
                    const double C1 = 0.0362;
                    const double C2 = 1.0937;
                    const double C3 = 25.7240;

                    return Math.Pow(GOR / (C1 * SGg_gs * Math.Exp(C3 * SGo_API / (T + Constants.F_TO_R))), 1.0 / C2);
                }
                else
                {
                    const double C1 = 0.0178;
                    const double C2 = 1.1870;
                    const double C3 = 23.931;

                    return Math.Pow(GOR / (C1 * SGg_gs * Math.Exp(C3 * SGo_API / (T + Constants.F_TO_R))), 1.0 / C2);
                }
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Glaso(double GOR,
                                       double T,
                                       double SGo_API,
                                       double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                const double a = 0.816;
                const double b = 0.172;
                const double c = -0.989;

                double pbst = Math.Pow(GOR / SGg, a) * Math.Pow(T, b) * Math.Pow(SGo_API, c);

                double log10pb = 1.7669 + 1.7447 * Math.Log10(pbst) - 0.30218 * Math.Pow(Math.Log10(pbst), 2.0);

                return Math.Pow(10.0, log10pb);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Marhoun(double GOR,
                                         double P,
                                         double T,
                                         double SGo_API,
                                         double SGg)
            {
                // This correlation assumes T is in degF
                T = T + Constants.F_TO_R;

                double Rs = GasSolubility.Marhoun(P, T, SGo_API, SGg);

                // Flag 4: Marhoun's Correlation
                double a       = 0.00538088;
                double b       = 0.715082;
                double c       = -1.87784;
                double d       = 3.1437;
                double e       = 1.32657;
                double gamma_o = 141.5 / (SGo_API + 131.5);

                return a * (Math.Pow(Rs, b)) * (Math.Pow(SGg, c)) * (Math.Pow(gamma_o, d)) * (Math.Pow(T, e));
            }
        }

        public static class OilCompressibility
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double Petrosky(double Rs,
                                          double P,
                                          double T,
                                          double SGo_API,
                                          double SGg)
            {
                // This correlation uses temperature in degF
                T = T + Constants.F_TO_R;

                const double a = 1.705e-7;
                const double b = 0.69357;
                const double c = 0.1885;
                const double d = 0.3272;
                const double e = 0.6729;
                const double f = -0.5906;

                return a * Math.Pow(Rs, b) * Math.Pow(SGg, c) * Math.Pow(SGo_API, d) * Math.Pow(T, e) * Math.Pow(P, f);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double VasquezBeggs(double Rs,
                                              double P,
                                              double T,
                                              double SGo_API,
                                              double SGg)
            {
                // VB correlation uses temperature in degF
                T = T + Constants.F_TO_R;

                // Correct gas specific gravity
                double SGg_gs = GasSolubility.correctGasSGVB(SGg, SGo_API, Constants.PSEPVB_DEF, Constants.TSEPVB_DEF);

                // Coefficients for c_o
                const double a1 = -1433;
                const double a2 = 5.0;
                const double a3 = 17.2;
                const double a4 = -1180;
                const double a5 = 12.61;
                const double a6 = 1e5;

                return (a1 + a2 * Rs + a3 * T + a4 * SGg_gs + a5 * SGo_API) / (a6 * P);
            }
        }

        public static class GasCompressibility
        {
            public static Tuple<double, double> pseudoPresTemp(double SGg,
                                                               double yCO2,
                                                               double yN2,
                                                               double yH2S)
            {
                // Compute pure hydrocarbon pc values
                double TpcBase = 169.2 + 349.5 * SGg - 74.0 * SGg * SGg;
                double PpcBase = 756.8 - 131.0 * SGg - 3.60 * SGg * SGg;

                // Get correction factor
                double A = yCO2 + yH2S;
                double B = yH2S;

                double ep = 120 * (Math.Pow(A, 0.9) - Math.Pow(A, 1.6)) + 15.0 * (Math.Pow(B, 0.5) - Math.Pow(B, 4.0));

                // Corrected properties
                double Tpc = TpcBase - ep;
                double Ppc = PpcBase * Tpc / (TpcBase + B * (1 - B) * ep);

                return new Tuple<double, double>(Ppc, Tpc);
            }

            public static double ZFactor(double P,
                                         double T,
                                         double SGg)
            {
                const int    maxIter = 100;
                const double tol     = 1E-10;

                // Compute pseudocritical temperature and pressure
                double Ppc;
                double Tpc;

                (Ppc, Tpc) = pseudoPresTemp(SGg, 0.0, 0.0, 0.0); // just use 0 for now for impurities

                // Compute pseudo-reduced temperature and pressure
                double Ppr = P / Ppc;
                double Tpr = T / Tpc;
                double t   = 1 / Tpr;
                // Compute reduced density, Y
                //---Constants
                const double A1  = 0.3265;
                const double A2  = -1.07;
                const double A3  = -0.5339;
                const double A4  = 0.01569;
                const double A5  = -0.05165;
                const double A6  = 0.5475;
                const double A7  = -0.7361;
                const double A8  = 0.1844;
                const double A9  = 0.1056;
                const double A10 = 0.6134;
                const double A11 = 0.7210;

                const double Zc = 0.27; // Assumed critical compressibility factor

                double R1 = A1 + A2 * t + A3 * t * t * t + A4 * t * t * t * t + A5 * t * t * t * t * t;

                double R2 = Zc * Ppr * t;

                double R3 = A6 + A7 * t + A8 * t * t;

                double R4 = A9 * (A7 * t + A8 * t * t);

                double R5 = A10 * t * t * t;

                //---Initial guess for Y assumes Z = 1
                double Y = Zc * Ppr / Tpr;

                //---Enter our iteration loop (Newton-Raphson)
                for(int n = 0; n < maxIter; ++n)
                {
                    double Yk = Y;
                    double Y2 = Y * Y, Y3 = Y2 * Y, Y4 = Y3 * Y, Y5 = Y4 * Y;
                    double F  = 1 - R2 / Y + R1 * Y + R3 * Y2 - R4 * Y5 + R5 * Y2 * (1 + A11 * Y2) * Math.Exp(-A11 * Y2);

                    double dF = R1                                      +
                                R2                                 / Y2 +
                                2 * R3                             * Y  +
                                2 * A11 * Math.Exp(-A11 * Y2) * R5 * Y3 -
                                5 * R4 * Y4 +
                                2 * Math.Exp(-A11 * Y2) * R5 * Y * (1 + A11 * Y2) -
                                2 * A11 * Math.Exp(-A11 * Y2) * R5 * Y3 * (1 + A11 * Y2);

                    double dY = -F / dF;

                    Y = Yk + dY;

                    if(Math.Abs(dY) < tol && Math.Abs(F) < tol)
                    {
                        break;
                    }
                }

                double Z = Zc * Ppr / (Tpr * Y);

                return Z;
            }

            public static double ZFactor(double Ppr,
                                         double Tpr)
            {
                int    maxIter = 100;
                double tol     = 1E-10;

                double t = 1 / Tpr;
                // Compute reduced density, Y
                //---Constants
                const double A1  = 0.3265;
                const double A2  = -1.07;
                const double A3  = -0.5339;
                const double A4  = 0.01569;
                const double A5  = -0.05165;
                const double A6  = 0.5475;
                const double A7  = -0.7361;
                const double A8  = 0.1844;
                const double A9  = 0.1056;
                const double A10 = 0.6134;
                const double A11 = 0.7210;

                const double Zc = 0.27; // Assumed critical compressibility factor

                double R1 = A1 + A2 * t + A3 * t * t * t + A4 * t * t * t * t + A5 * t * t * t * t * t;

                double R2 = Zc * Ppr * t;

                double R3 = A6 + A7 * t + A8 * t * t;

                double R4 = A9 * (A7 * t + A8 * t * t);

                double R5 = A10 * t * t * t;

                //---Initial guess for Y assumes Z = 1
                double Y = Zc * Ppr / Tpr;

                //---Enter our iteration loop (Newton-Raphson)
                for(int n = 0; n < maxIter; ++n)
                {
                    double Yk = Y;
                    double Y2 = Y * Y, Y3 = Y2 * Y, Y4 = Y3 * Y, Y5 = Y4 * Y;
                    double F  = 1 - R2 / Y + R1 * Y + R3 * Y2 - R4 * Y5 + R5 * Y2 * (1 + A11 * Y2) * Math.Exp(-A11 * Y2);

                    double dF = R1                                      +
                                R2                                 / Y2 +
                                2 * R3                             * Y  +
                                2 * A11 * Math.Exp(-A11 * Y2) * R5 * Y3 -
                                5 * R4 * Y4 +
                                2 * Math.Exp(-A11 * Y2) * R5 * Y * (1 + A11 * Y2) -
                                2 * A11 * Math.Exp(-A11 * Y2) * R5 * Y3 * (1 + A11 * Y2);

                    double dY = -F / dF;

                    Y = Yk + dY;

                    if(Math.Abs(dY) < tol && Math.Abs(F) < tol)
                    {
                        break;
                    }
                }

                double Z = Zc * Ppr / (Tpr * Y);

                return Z;
            }
        }

        public static class OilViscosity
        {
            public static class Undersaturated
            {
                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double PetroskyFarshad(double mu_ob,
                                                     double P,
                                                     double Pb)
                {
                    double A = -1.0146 + 1.3322 * Math.Log10(mu_ob) - 0.4876 * Math.Pow(Math.Log10(mu_ob), 2.0) - 1.15036 * Math.Pow(Math.Log10(mu_ob), 3.0);

                    return mu_ob + 1.3499e-3 * (P - Pb) * Math.Pow(10.0, A);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double VasquezBeggs(double mu_ob,
                                                  double P,
                                                  double Pb)
                {
                    const double C1 = 2.6;
                    const double C2 = 1.187;
                    const double C3 = -11.513;
                    const double C4 = -8.98e-5;

                    double m = C1 * Math.Pow(P, C2) * Math.Exp(C3 + C4 * P);

                    return mu_ob * Math.Pow(P / Pb, m);
                }
            }

            public static class Saturated
            {
                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double PetroskyFarshad(double mu_od,
                                                     double Rs)
                {
                    double A = 0.1651 + 0.6165 * Math.Pow(10.0, -6.0866e-4 * Rs);
                    double B = 0.5131 + 0.5109 * Math.Pow(10.0, -1.1831e-3 * Rs);

                    return A * Math.Pow(mu_od, B);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double BeggsRobinson(double mu_od,
                                                   double Rs)
                {
                    double A = 10.715 * Math.Pow(Rs + 100.0, -0.515);
                    double B = 5.44   * Math.Pow(Rs + 150.0, -0.338);

                    return A * Math.Pow(mu_od, B);
                }
            }

            public static class Dead
            {
                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double BeggsRobinson(double T,
                                                   double SGo_API)
                {
                    // This correlation assumes T is in degF
                    T = T + Constants.F_TO_R;

                    double Z = 3.0324 - 0.02023 * SGo_API;
                    double Y = Math.Pow(10.0, Z);
                    double X = Y * Math.Pow(T, -1.163);

                    return Math.Pow(10.0, X) - 1.0;
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double PetroskyFarshad(double T,
                                                     double SGo_API)
                {
                    // This correlation assumes T is in degF
                    T = T + Constants.F_TO_R;

                    double X = 4.59388 * Math.Log10(T) - 22.82792;

                    return 2.3511e7 * Math.Pow(T, -2.10255) * Math.Pow(Math.Log10(SGo_API), X);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double Glaso(double T,
                                           double SGo_API)
                {
                    // Glaso correlation uses temperature in degF
                    T = T + Constants.F_TO_R;

                    double c = 3.141e10 * Math.Pow(T, -3.444), d = 10.313 * Math.Log10(T) - 36.447;

                    return c * Math.Pow(Math.Log10(SGo_API), d);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
                public static double Beal(double T,
                                          double SGo_API)
                {
                    // This correlation assumes T is in degF
                    T = T + Constants.F_TO_R;

                    double A = Math.Pow(10.0, 0.43 + 8.33 / SGo_API);

                    return (0.32 + 1.8e7 / Math.Pow(SGo_API, 4.53)) * Math.Pow(360.0 / (T + 200.0), A);
                }
            }
        }

        public static class WaterViscosity
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
            public static double McCain(double P,
                                        double T,
                                        double S = 0.0)
            {
                // Convert temperature to degF
                T = T + Constants.F_TO_R;

                double S2 = S  * S;
                double S3 = S2 * S;
                double S4 = S3 * S;

                // compute viscosity at atmospheric pressure and given temperature and salinity.
                double A   = 109.574 - 8.40564 * S + 0.313314 * S2                        + 8.72213e-3 * S3;
                double B   = 1.12166 - 2.63951e-2 * S + 6.79461e-4 * S2 + 5.47119e-5 * S3 - 1.55586e-6 * S4;
                double mu1 = A * Math.Pow(T, -B);

                // Correct viscosity for pressure
                return mu1 * (0.9994 + 4.0295e-5 * P + 3.1062e-9 * P * P);
            }
        }

        public static class GasViscosity
        {
            public static double CarrKobayashiBurrows(double P,
                                                      double T,
                                                      double SGg,
                                                      double yCO2 = 0.0,
                                                      double yN2  = 0.0,
                                                      double yH2S = 0.0)
            {
                // This correlation is in terms of Fahrenheit
                T = T + Constants.F_TO_R;

                // Compute viscosity at atmospheric pressure and given temperature
                double mu1uc  = (1.709e-5 - 2.062e-6 * SGg) * T + 8.118e-3 - 6.15e-3 * Math.Log10(SGg);
                double dmuCO2 = yCO2 * (9.08e-3 * Math.Log10(SGg) + 6.24e-3);
                double dmuN2  = yN2  * (8.48e-3 * Math.Log10(SGg) + 9.59e-3);
                double dmuH2S = yH2S * (8.49e-3 * Math.Log10(SGg) + 3.73e-3);

                double mu1 = mu1uc + dmuCO2 + dmuN2 + dmuH2S;

                // Compute viscosity ratio term
                // Constants
                const double a0  = -2.46211820;
                const double a1  = 2.970547414;
                const double a2  = -2.86264054e-1;
                const double a3  = 8.05420522e-3;
                const double a4  = 2.80860949;
                const double a5  = -3.49803305;
                const double a6  = 3.60373020e-1;
                const double a7  = -1.044324e-2;
                const double a8  = -7.93385648e-1;
                const double a9  = 1.39643306;
                const double a10 = -1.49144925e-1;
                const double a11 = 4.41015512e-3;
                const double a12 = 8.39387178e-2;
                const double a13 = -1.86408848e-1;
                const double a14 = 2.03367881e-2;
                const double a15 = -6.09579263e-4;

                // Compute pseudocritical and pseudoreduced pressure and temperature
                double Ppc, Tpc;
                (Ppc, Tpc) = GasCompressibility.pseudoPresTemp(SGg, yCO2, yN2, yH2S); // Should we use 0 for the y's here (since we corrected for them above???)

                double Ppr = P / Ppc, Tpr = (T + Constants.F_TO_R) / Tpc;

                double Ppr2 = Ppr * Ppr, Ppr3 = Ppr2 * Ppr;
                double Tpr2 = Tpr * Tpr, Tpr3 = Tpr2 * Tpr;

                double R = a0                                                 +
                           a1   * Ppr                                         +
                           a2   * Ppr2                                        +
                           a3   * Ppr3                                        +
                           Tpr  * (a4 + a5 * Ppr + a6 * Ppr2    + a7  * Ppr3) +
                           Tpr2 * (a8 + a9 * Ppr + a10 * Ppr2   + a11 * Ppr3) +
                           Tpr3 * (a12 + a13 * Ppr + a14 * Ppr2 + a15 * Ppr3);

                double mu = Math.Exp(R) * mu1 / Tpr;

                return mu;
            }

            public static double LeeGonzalezEakin(double P,
                                                  double T,
                                                  double SGg,
                                                  double yCO2 = 0.0,
                                                  double yN2  = 0.0,
                                                  double yH2S = 0.0)
            {
                // Compute z-factor
                double z = GasCompressibility.ZFactor(P, T, SGg);

                // Compute density
                double R    = Constants.RGAS;               // psi ft^3/ lbmol R
                double Mg   = Constants.STDAIRMA   * SGg;   // lb/lbmol
                double rhog = P * Mg / (z * R * T) / 62.37; // gm/cc

                // Compute gas viscosity
                double K = (9.4 + 0.02 * Mg)      * Math.Pow(T, 1.5) / (209.0 + 19.0 * Mg + T);
                double X = 3.5 + 986.0 / T + 0.01 * Mg;
                double Y = 2.4             - 0.2  * X;

                return 1e-4 * K * Math.Exp(X * Math.Pow(rhog, Y));
            }
        }

        #region Methods

        /// Commonly used functions.
        /// Convert oil specific gravity (unitless) to degAPI
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double SG_to_API(double SG)
        {
            return 141.5 / SG - 131.5;
        }

        // convert oil specific gravity (degAPI) to unitless version
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double API_to_SG(double SG_API)
        {
            return 141.5 / (SG_API + 131.5);
        }

        // Convert GLR and WC to GOR and WOR
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void GLR_WC_to_GOR_WOR(ref double GOR,
                                             ref double WOR,
                                             double     GLR,
                                             double     WC)
        {
            WOR = WC / (1.0 - WC);

            GOR = GLR * (WOR + 1.0);
        }

        // Convert GOR and WOR to GLR and WC
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void GOR_WOR_to_GLR_WC(ref double GLR,
                                             ref double WC,
                                             double     GOR,
                                             double     WOR)
        {
            WC = WOR / (WOR + 1.0);

            GLR = GOR / (WOR + 1.0);
        }

        // Convert GLR and WOR to GOR and WC
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void GLR_WOR_to_GOR_WC(ref double GOR,
                                             ref double WC,
                                             double     GLR,
                                             double     WOR)
        {
            WC = WOR / (WOR + 1.0);

            GOR = GLR * (WOR + 1.0);
        }

        // Convert GOR and WC to GLR and WOR
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static void GOR_WC_to_GLR_WOR(ref double GLR,
                                             ref double WOR,
                                             double     GOR,
                                             double     WC)
        {
            WOR = WC / (1.0 - WC);

            GLR = GOR / (WOR + 1.0);
        }

        /// <summary>
        ///     Chen correlation for Darcy-Weissbach friction factor. Inputs are the Reynolds number and the relative rougness
        ///     factor e/D
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double frictionFactor(double Re,
                                            double eD)
        {
            if(Re < 2300)
            {
                return 64.0 / Re;
            }

            double F = -2 * Math.Log10(eD / 3.7065 - 5.0452 / Re * Math.Log10(1 / 2.8257 * Math.Pow(eD, 1.1098) + 5.8506 / Math.Pow(Re, 0.8981)));

            return 1.0 / F / F;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static Tuple<double, double> frictionFactorWithJac(double Re,
                                                                  double eD)
        {
            double f, df_dRe;

            if(Re < 2300)
            {
                f      = 64.0       / Re;
                df_dRe = -64.0 / Re / Re;
            }

            else
            {
                double B  = Math.Pow(eD, 1.1098) / 2.8257 + 5.8506      / Math.Pow(Re, 0.8981);
                double A  = eD                   / 3.7065 - 5.0452 / Re * Math.Log10(B);
                double dB = -5.25442 / Math.Pow(Re, 1.8981);
                double dA = -5.0452  * (-Math.Log10(B) / Re / Re + 1 / (B * Re * Math.Log(10.0)) * dB);

                f = Math.Pow(-2.0 * Math.Log10(A), -2.0);

                df_dRe = 4.0 * Math.Pow(-2.0 * Math.Log10(A), -3.0) * 1.0 / (A * Math.Log(10.0)) * dA;
            }

            return new Tuple<double, double>(f, df_dRe);
        }

        #endregion
    }

    // public static class HydrocarbonProperties
    // {
    //     public static double mu_o(double P,
    //                               double Pb,
    //                               double mu_ob)
    //     {
    //         double a = -0.000039 * P - 5;
    //         double m = 2.6 * (Math.Pow(P, 1.187)) * (Math.Pow(10, a));
    //
    //         return mu_ob * Math.Pow((P / Pb), m);
    //     }
    //
    //     public static double mu_osat(double Rs,
    //                                  double mu_od,
    //                                  double flag)
    //     {
    //         // This function will calculate the viscosity of oil (in cp)
    //         // at bubble point pressure from dead oil viscosity
    //         // Required data are:
    //         // Rs = Solution GOR, SCF/STB
    //         // mu_od = Dead oil viscosity, cp
    //         // flag = 1 for Chew-Connally Correlation
    //         // = 2 for Beggs-Robinson Correlation
    //         if((flag == 1))
    //         {
    //             // The Chew-Connally Correlation
    //             double a = Rs        * (22000000 * Rs - 0.00074);
    //             double c = 0.0000862 * Rs;
    //             double d = 0.0011    * Rs;
    //             double e = 0.00374   * Rs;
    //             double b = 0.68 / (Math.Pow(10, c)) + 0.25 / (Math.Pow(10, d)) + 0.062 / (Math.Pow(10, e));
    //
    //             return (Math.Pow(10, a)) * (Math.Pow(mu_od, b));
    //         }
    //         else
    //         {
    //             // The Beggs-Robinson Correlation
    //             double a = 10.715 * Math.Pow((Rs + 100), (-0.515));
    //             double b = 5.44   * Math.Pow((Rs + 150), (-0.338));
    //
    //             return a * (Math.Pow(mu_od, b));
    //         }
    //     }
    //
    //     public static double mu_odead(double T,
    //                                   double API,
    //                                   double flag)
    //     {
    //         // It calculates dead oil viscosity in cp.
    //         // Required data are:
    //         // T =Temperature in R
    //         // API = Oil density in degree API
    //
    //         if((flag == 1))
    //         {
    //             // Beal's Correlation
    //             double a = Math.Pow(10, (0.43 + 8.33 / API));
    //
    //             return (0.32 + 18000000.0 / (Math.Pow(API, 4.53))) * Math.Pow((360 / (T - 260)), a);
    //         }
    //
    //         {
    //             if((flag == 2))
    //             {
    //                 // The Beggs-Robinson Correlation
    //                 double Z = 3.0324 - 0.02023 * API;
    //                 double Y = Math.Pow(10, Z);
    //                 double x = Y * Math.Pow((T - 460), (-1.163));
    //
    //                 return Math.Pow(10, x) - 1;
    //             }
    //
    //             // Glaso's Corelation
    //             double a = 10.313 * (Math.Log(T - 460) / Math.Log(10)) - 36.447;
    //
    //             return 31410000000.0 * Math.Pow((T - 460), (-3.444)) * Math.Pow((Math.Log(API) / Math.Log(10)), a);
    //         }
    //     }
    //
    //     public enum OilFormationVolumeFactorCorrelations
    //     {
    //         Standing,
    //         VasquezBeggs,
    //         Glaso,
    //         Marhoun,
    //         PetroskyFarshad,
    //         MaterialBalanceEquation
    //     }
    //
    //     public static double Bo(double P,
    //                             double T,
    //                             double gamma_g,
    //                             double API,
    //                             double Rs,
    //                             double flag)
    //     {
    //         double gamma_o = 141.5 / (API + 131.5);
    //
    //         if((flag == 1))
    //             // Standing Correlation
    //             return 0.9759 + 0.00012 * Math.Pow((Rs * Math.Pow(gamma_g / gamma_o, 2) + 1.25 * (T - 460)), 1.2);
    //
    //         if((flag == 2))
    //         {
    //             double C1, C2, C3;
    //
    //             // Flag 2: The Vasquez-Beggs Correlation
    //             if((API > 30))
    //             {
    //                 C1 = 0.000467;
    //                 C2 = 0.000011;
    //                 C3 = 0.000000001337;
    //             }
    //             else
    //             {
    //                 C1 = 0.0004677;
    //                 C2 = 0.00001751;
    //                 C3 = -0.00000001811;
    //             }
    //
    //             double gamma_gs = gamma_g * (1 + 0.00005912 * API * (T - 460) * (Math.Log(P / 114.7) / Math.Log(10)));
    //
    //             return 1.0 + C1 * Rs + (T - 520) * (API / gamma_gs) * (C2 + C3 * Rs);
    //         }
    //
    //         if((flag == 3))
    //         {
    //             // Flag 3: Glaso's Correlation
    //             double B_ob_star     = Rs * Math.Pow((gamma_g / gamma_o), 0.526) + 0.968 * (T - 460);
    //             double log_B_ob_star = Math.Log(B_ob_star) / Math.Log(10);
    //             double Aa            = -6.58511 + 2.91329 * log_B_ob_star - 0.27683 * log_B_ob_star * log_B_ob_star;
    //
    //             return 1.0 + Math.Pow(10, Aa);
    //         }
    //
    //         if((flag == 4))
    //         {
    //             // Flag 4: Marhoun's Correlation
    //             double a = 0.74239;
    //             double b = 0.323294;
    //             double c = -1.20204;
    //             double F = (Math.Pow(Rs, a)) * (Math.Pow(gamma_g, b)) * (Math.Pow(gamma_o, c));
    //
    //             return 0.497069 + 0.000862963 * T + 0.00182594 * F;
    //         }
    //
    //         if((flag == 5))
    //             // Flag 5: The Petrosky-Farshad Correlation
    //             return 1.0113 + 0.000072046 * Math.Pow(((Math.Pow(Rs, 0.3738)) * ((Math.Pow(gamma_g, 0.2914)) / Math.Pow(gamma_o, 0.6265)) + 0.24626 * Math.Pow((T - 460), 0.5371)), 3.0936);
    //
    //         // Flag 6: Material Balance Equation
    //         double rho_o = gamma_o * 62.4;
    //
    //         return (62.4 * gamma_o + 0.0136 * Rs * gamma_g) / rho_o;
    //     }
    //
    //     public enum BubblePoinPressureCorrelations
    //     {
    //         Standing,
    //         VasquezBeggs,
    //         Glaso,
    //         Marhoun,
    //         PetroskyFarshad
    //     }
    //
    //     public static double Pb(double P,
    //                             double T,
    //                             double gamma_g,
    //                             double API,
    //                             double Rs,
    //                             double flag)
    //     {
    //         double Rs_by_gamma_g = Rs / gamma_g;
    //
    //         if((flag == 1))
    //         {
    //             // Standing Correlation
    //             double a = 0.00091 * (T - 460) - 0.0125 * API;
    //
    //             return 18.2 * (Math.Pow(Rs_by_gamma_g, 0.83) * Math.Pow(10, a) - 1.4);
    //         }
    //
    //         if((flag == 2))
    //         {
    //             double C1, C2, C3;
    //
    //             // Flag 2: The Vasquez-Beggs Correlation
    //             if((API > 30))
    //             {
    //                 C1 = 56.18;
    //                 C2 = 0.84246;
    //                 C3 = 10.393;
    //             }
    //             else
    //             {
    //                 C1 = 27.624;
    //                 C2 = 0.914328;
    //                 C3 = 11.172;
    //             }
    //
    //             double a        = -C3 * API / T;
    //             double gamma_gs = gamma_g   * (1 + 0.00005912 * API * (T - 460) * (Math.Log(P / 114.7) / 2.30258509299405));
    //
    //             return Math.Pow(((C1 * Rs_by_gamma_g) * Math.Pow(10, a)), C2);
    //         }
    //
    //         if((flag == 3))
    //         {
    //             // Flag 3: Glaso's Correlation
    //             double a           = 0.816;
    //             double b           = 0.172;
    //             double c           = -0.989;
    //             double pb_star     = (Math.Pow(Rs_by_gamma_g, a)) * (Math.Pow((T - 460), b)) * (Math.Pow(API, c));
    //             double log_pb_star = Math.Log(pb_star)                                       / 2.30258509299405;
    //             double log_pb      = 1.7669 + 1.7447 * log_pb_star - 0.30218 * Math.Pow(log_pb_star, 2);
    //
    //             return Math.Pow(10, log_pb);
    //         }
    //
    //         if((flag == 4))
    //         {
    //             // Flag 4: Marhoun's Correlation
    //             double a       = 0.00538088;
    //             double b       = 0.715082;
    //             double c       = -1.87784;
    //             double d       = 3.1437;
    //             double e       = 1.32657;
    //             double gamma_o = 141.5 / (API + 131.5);
    //
    //             return a * (Math.Pow(Rs, b)) * (Math.Pow(gamma_g, c)) * (Math.Pow(gamma_o, d)) * (Math.Pow(T, e));
    //         }
    //
    //         // Flag 5: The Petrosky-Farshad Correlation
    //         double x = 0.0007916 * (Math.Pow(API, 1.541)) - 0.00004561 * Math.Pow((T - 460), 1.3911);
    //
    //         return ((112.727 * (Math.Pow(Rs, 0.577421))) / ((Math.Pow(gamma_g, 0.8439)) * (Math.Pow(10, x)))) - 1391.051;
    //     }
    //
    //     public enum GasSolubilityCorrelations
    //     {
    //         Standing,
    //         VasquezBeggs,
    //         Glaso,
    //         Marhoun,
    //         PetroskyFarshad
    //     }
    //
    //     /// <summary>
    //     /// Standing Correlation is applicable at or below bubble point
    //     /// </summary>
    //     /// <param name="P"></param>
    //     /// <param name="T"></param>
    //     /// <param name="gamma_g"></param>
    //     /// <param name="API"></param>
    //     /// <param name="flag"></param>
    //     /// <returns></returns>
    //     public static double Rs(double P,
    //                             double T,
    //                             double gamma_g,
    //                             double API,
    //                             double flag)
    //     {
    //         if((flag == 1))
    //         {
    //             // Flag 1: Standing Correlation
    //             double x = 0.0125 * API - 0.00091 * (T - 460);
    //
    //             return gamma_g * Math.Pow(((P / 18.2 + 1.4) * Math.Pow(10, x)), 1.2048);
    //         }
    //
    //         if((flag == 2))
    //         {
    //             double C1, C2, C3;
    //
    //             // Flag 2: The Vasquez-Beggs Correlation
    //             if((API > 30))
    //             {
    //                 C1 = 0.0178;
    //                 C2 = 1.187;
    //                 C3 = 23.931;
    //             }
    //             else
    //             {
    //                 C1 = 0.0362;
    //                 C2 = 1.0937;
    //                 C3 = 25.724;
    //             }
    //
    //             double gamma_gs = gamma_g * (1 + 0.00005912 * API * (T - 460) * (Math.Log(P / 114.7) / 2.30258509299405));
    //
    //             return C1 * gamma_gs * (Math.Pow(P, C2)) * Math.Exp(C3 * (API / T));
    //         }
    //
    //         if((flag == 3))
    //         {
    //             // Flag 3: Glaso's Correlation
    //             double x       = 2.8869 - Math.Pow(14.1811 - 3.3093 * (Math.Log(P) / 2.30258509299405), 2);
    //             double pb_star = Math.Pow(10, x);
    //
    //             return gamma_g * Math.Pow(((Math.Pow(API, 0.989) / Math.Pow((T - 460), 0.172)) * pb_star), 1.2255);
    //         }
    //
    //         if((flag == 4))
    //         {
    //             // Flag 4: Marhoun's Correlation
    //             double a       = 185.843208;
    //             double b       = 1.87784;
    //             double c       = -3.1437;
    //             double d       = -1.32657;
    //             double e       = 1.398441;
    //             double gamma_o = 141.5 / (API + 131.5);
    //
    //             return Math.Pow((a * (Math.Pow(gamma_g, b)) * (Math.Pow(gamma_o, c)) * (Math.Pow(T, d)) * P), e);
    //         }
    //
    //         {
    //             // Flag 5: The Petrosky-Farshad Correlation
    //             double x = 0.0007916 * (Math.Pow(API, 1.541)) - 0.00004561 * Math.Pow((T - 460), 1.3911);
    //
    //             return Math.Pow(((P / 112.727 + 12.34) * (Math.Pow(gamma_g, 0.8439)) * (Math.Pow(10, x))), 1.73184);
    //         }
    //     }
    //
    //     public static double Bt(double P,
    //                             double T,
    //                             double gamma_g,
    //                             double API,
    //                             double Rs,
    //                             double flag)
    //     {
    //         double gamma_o = 141.5 / (API + 131.5);
    //
    //         if((flag == 1))
    //         {
    //             // Standing Correlation
    //             double c = 2.9 * Math.Pow(10, (-0.00027 * Rs));
    //
    //             double log_A_star = (Math.Log(Rs                         * ((Math.Pow((T - 460), 0.5)) * (Math.Pow(gamma_o, c))) / Math.Pow(gamma_g, 0.3)) / 2.30258509299405) -
    //                                 (10.1 - 96.8 / (6.604 + (Math.Log(P) / 2.30258509299405)));
    //
    //             double log_Bt = -5.223 - 47.4 / (-12.22 + log_A_star);
    //
    //             return Math.Pow(10, (log_Bt));
    //         }
    //
    //         if((flag == 2))
    //         {
    //             // Glaso's Correlation
    //             double c          = 2.9                                                                                   * Math.Pow(10, (-0.00027 * Rs));
    //             double A_star     = ((Rs * (Math.Pow((T - 460), 0.5)) * (Math.Pow(gamma_o, c))) / Math.Pow(gamma_g, 0.3)) * Math.Pow(P,  (-1.1089));
    //             double log_A_star = Math.Log(A_star)                                                                      / Math.Log(10);
    //             double log_Bt     = 0.080135 + 0.47257 * log_A_star + 0.17351 * log_A_star * log_A_star;
    //
    //             return Math.Pow(10, log_Bt);
    //         }
    //         else
    //         {
    //             // Marhoun's Correlation
    //             double a = 0.644516;
    //             double b = -1.07934;
    //             double c = 0.724874;
    //             double d = 2.00621;
    //             double e = -0.76191;
    //             double F = (Math.Pow(Rs, a)) * (Math.Pow(gamma_g, b)) * (Math.Pow(gamma_o, c)) * (Math.Pow(T, d)) * (Math.Pow(P, e));
    //
    //             return 0.314693 + 0.0000106253 * F + 0.000000000018883 * F * F;
    //         }
    //     }
    //
    //     public enum IsothermalCompressibilityOfOilCorrelations
    //     {
    //         VasquezBeggs,
    //         Marhoun,
    //         McCainNoPb,
    //         McCainWithPb
    //     }
    //
    //     public static double Co(double P,
    //                             double T,
    //                             double gamma_g,
    //                             double API,
    //                             double Rsb,
    //                             double flag)
    //     {
    //         double gamma_o  = 141.5   / (API + 131.5);
    //         double gamma_gs = gamma_g * (1   + 0.00005912 * API * (T - 460) * (Math.Log(P / 114.7) / 2.30258509299405));
    //
    //         if((flag == 1))
    //             // The Vasquez-Beggs Correlation
    //             return (-1433 + 5 * Rsb + 17.2 * (T - 460) - 1180 * gamma_gs + 12.61 * API) / (P * 100000);
    //
    //         if((flag == 2))
    //             // The Petrosky-Farshad Correlation
    //             return 0.0000001705 * (Math.Pow(Rsb, 0.69357)) * (Math.Pow(gamma_g, 0.1885)) * (Math.Pow(API, 0.3272)) * (Math.Pow((T - 460), 0.6729)) * (Math.Pow(P, (-0.5906)));
    //
    //         // McCain Correlation without bubble point pressure
    //         double Aa = -7.633 - 1.497 * Math.Log(P) + 1.115 * Math.Log(T) + 0.533 * Math.Log(API) + 0.184 * Math.Log(Rsb);
    //
    //         return Math.Exp(Aa);
    //     }
    //
    //     public static double Co_Pb(double P,
    //                                double Pb,
    //                                double T,
    //                                double gamma_g,
    //                                double API,
    //                                double Rsb)
    //     {
    //         // McCain's Correlation with known bubble point pressure
    //         double Aa = -7.573 - 1.45 * Math.Log(P) - 0.383 * Math.Log(Pb) + 1.402 * Math.Log(T) + 0.256 * Math.Log(API) + 0.449 * Math.Log(Rsb);
    //
    //         return Math.Exp(Aa);
    //     }
    // }
}
