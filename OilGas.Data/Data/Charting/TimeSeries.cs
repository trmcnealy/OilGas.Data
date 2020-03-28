using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;

namespace OilGas.Data.Charting
{
    public static class TimeSeries
    {
        private static readonly int[][] daysInMonth =
        {
            new int[]
            {
                31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            },
            new int[]
            {
                31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
            }
        };

        private static readonly int[][] daysInYear =
        {
            new int[]
            {
                31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            },
            new int[]
            {
                31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
            }
        };

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int DaysInMonth(this DateTime dateTime)
        {
            return daysInMonth[dateTime.Year % 4][(dateTime.Month - 1) % 12];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static int DaysInMonth(int year,
                                      int month)
        {
            return daysInMonth[year % 4][(month - 1) % 12];
        }

        //public static DateTime AddMonths(this DateTime dateTime,
        //                                 int           months)
        //{
        //    int day   = dateTime.Day;
        //    int month = dateTime.Month;
        //    int year  = dateTime.Year;

        //    int dayOffset = dateTime.DayOfYear;

        //    int daysInCurrentMonth = DaysInMonth(year,
        //                                         month);

        //    int daysUntilEndOfMonth = daysInCurrentMonth - day;

        //    for(int i = 0; i < months; i++)
        //    {
        //        dayOffset += DaysInMonth(year,
        //                                 month + i);
        //    }

        //    return dateTime.AddMonths();
        //}
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static IEnumerable<double> MovingAverage(this IEnumerable<double> source,
                                                        int                      blockSize)
        {
            if(source == null)
            {
                throw new ArgumentNullException(nameof(source));
            }

            if(blockSize < 2)
            {
                throw new Exception("blockSize");
            }

            double sum   = 0;
            int    block = blockSize;
            int    nans  = -1;

            using IEnumerator<double> left = source.GetEnumerator();

            using IEnumerator<double> right = source.GetEnumerator();

            double value;

            while(block > 1)
            {
                block--;

                if(!right.MoveNext())
                {
                    if(nans > 0)
                    {
                        yield return double.NaN;
                    }
                    else
                    {
                        yield return sum / (blockSize - block - 1);
                    }

                    yield break;
                }

                value = right.Current;

                if(double.IsNaN(value))
                {
                    nans = blockSize;
                }
                else
                {
                    sum += value;
                    nans--;
                }
            }

            while(right.MoveNext())
            {
                value = right.Current;

                if(double.IsNaN(value))
                {
                    nans = blockSize;
                }
                else
                {
                    sum += value;
                    nans--;
                }

                if(nans > 0)
                {
                    yield return double.NaN;
                }
                else
                {
                    yield return sum / blockSize;
                }

                left.MoveNext();
                value = left.Current;

                if(!double.IsNaN(value))
                {
                    sum -= value;
                }
            }
        }
        
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static IEnumerable<double> WeightedMovingAverage(this IEnumerable<double> source,
                                                                int                      blockSize,
                                                                Func<int, double>        weight)
        {
            if(source == null)
            {
                throw new ArgumentNullException(nameof(source));
            }

            if(weight == null)
            {
                throw new ArgumentNullException(nameof(weight));
            }

            if(blockSize < 2)
            {
                throw new Exception("blockSize");
            }

            Queue<double> current = new Queue<double>();
            double[]      factor  = new double[blockSize];

            using IEnumerator<double> se = source.GetEnumerator();

            for(int i = 0; i < blockSize - 1; i++)
            {
                if(!se.MoveNext())
                {
                    throw new Exception("source");
                }

                current.Enqueue(se.Current);
                factor[i] = weight(i + 1);
            }

            factor[blockSize - 1] = weight(blockSize);
            double factorSum = factor.Sum();

            Func<T, T, T> InitMultiply<T>()
            {
                Type type = typeof(T);

                ParameterExpression left = Expression.Parameter(type,
                                                                "left");

                ParameterExpression right = Expression.Parameter(type,
                                                                 "right");

                return Expression.Lambda<Func<T, T, T>>(Expression.Multiply(left,
                                                                            right),
                                                        left,
                                                        right).Compile();
            }

            IEnumerable<T> Multiply<T>(IEnumerable<T> leftSequence,
                                       IEnumerable<T> rightSequence)
            {
                Func<T, T, T> multiply = InitMultiply<T>();

                using IEnumerator<T> l = leftSequence.GetEnumerator();

                using IEnumerator<T> r = rightSequence.GetEnumerator();

                while(l.MoveNext() && r.MoveNext())
                {
                    yield return multiply(l.Current,
                                          r.Current);
                }
            }

            while(se.MoveNext())
            {
                current.Enqueue(se.Current);

                yield return Multiply(current,
                                      factor).Sum() /
                             factorSum;

                current.Dequeue();
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
        public static double[] Smooth(double[] xval,
                                      double[] yval,
                                      double   bandwidth       = 0.3,
                                      int      robustnessIters = 2)
        {
            if(xval.Length != yval.Length)
            {
                throw new Exception($"Loess expects the abscissa and ordinate arrays to be of the same size, but got {xval.Length} abscisssae and {yval.Length} ordinatae");
            }

            int n = xval.Length;

            if(n == 0)
            {
                throw new Exception("Loess expects at least 1 point");
            }

            if(n == 1)
            {
                return new double[]
                {
                    yval[0]
                };
            }

            if(n == 2)
            {
                return new double[]
                {
                    yval[0], yval[1]
                };
            }

            int bandwidthInPoints = (int)(bandwidth * n);

            if(bandwidthInPoints < 2)
            {
                throw new
                    Exception($"the bandwidth must be large enough to accomodate at least 2 points. There are {n} data points, and bandwidth must be at least {2.0 / n} but it is only {bandwidth}");
            }

            double[] res = new double[n];

            double[] residuals       = new double[n];
            double[] sortedResiduals = new double[n];

            double[] robustnessWeights = new double[n];

            double x;
            int[]  bandwidthInterval;

            int ileft;
            int iright;

            // Compute the point of the bandwidth interval that is
            // farthest from x
            int edge;

            double sumWeights;
            double sumX;
            double sumXSquared;
            double sumY;
            double sumXY;
            double denom;

            double xk;
            double yk;
            double dist;
            double w;
            double xkw;

            double meanX;
            double meanY;
            double meanXY;
            double meanXSquared;
            double beta;

            double alpha;
            double arg;
            double medianResidual;

            void UpdateBandwidthInterval(double[] _xval,
                                         int      i,
                                         int[]    _bandwidthInterval)
            {
                int left  = _bandwidthInterval[0];
                int right = _bandwidthInterval[1];

                // The edges should be adjusted if the previous point to the
                // left is closer to x than the current point to the right or
                // if the next point to the right is closer
                // to x than the leftmost point of the current interval
                if(left != 0 && _xval[i] - _xval[left - 1] < _xval[right] - _xval[i])
                {
                    _bandwidthInterval[0]++;
                    _bandwidthInterval[1]++;
                }
                else if(right < _xval.Length - 1 && _xval[right + 1] - _xval[i] < _xval[i] - _xval[left])
                {
                    _bandwidthInterval[0]++;
                    _bandwidthInterval[1]++;
                }
            }

            double TriCube(double _x)
            {
                double tmp = Math.Abs(_x);
                tmp = 1 - tmp * tmp * tmp;

                return tmp * tmp * tmp;
            }

            // Do an initial fit and 'robustnessIters' robustness iterations.
            // This is equivalent to doing 'robustnessIters+1' robustness iterations
            // starting with all robustness weights set to 1.
            for(int i = 0; i < robustnessWeights.Length; i++)
            {
                robustnessWeights[i] = 1;
            }

            for(int iter = 0; iter <= robustnessIters; ++iter)
            {
                bandwidthInterval = new int[]
                {
                    0, bandwidthInPoints - 1
                };

                // At each x, compute a local weighted linear regression
                for(int i = 0; i < n; ++i)
                {
                    x = xval[i];

                    // Find out the interval of source points on which
                    // a regression is to be made.
                    if(i > 0)
                    {
                        UpdateBandwidthInterval(xval,
                                                i,
                                                bandwidthInterval);
                    }

                    ileft  = bandwidthInterval[0];
                    iright = bandwidthInterval[1];

                    // Compute the point of the bandwidth interval that is
                    // farthest from x

                    if(xval[i] - xval[ileft] > xval[iright] - xval[i])
                    {
                        edge = ileft;
                    }
                    else
                    {
                        edge = iright;
                    }

                    // Compute a least-squares linear fit weighted by
                    // the product of robustness weights and the tricube
                    // weight function.
                    // See http://en.wikipedia.org/wiki/Linear_regression
                    // (section "Univariate linear case")
                    // and http://en.wikipedia.org/wiki/Weighted_least_squares
                    // (section "Weighted least squares")
                    sumWeights  = 0;
                    sumX        = 0;
                    sumXSquared = 0;
                    sumY        = 0;
                    sumXY       = 0;
                    denom       = Math.Abs(1.0f / (xval[edge] - x));

                    for(int k = ileft; k <= iright; ++k)
                    {
                        xk = xval[k];
                        yk = yval[k];

                        if(k < i)
                        {
                            dist = x - xk;
                        }
                        else
                        {
                            dist = xk - x;
                        }

                        w   = TriCube(dist * denom) * robustnessWeights[k];
                        xkw = xk                    * w;

                        sumWeights  += w;
                        sumX        += xkw;
                        sumXSquared += xk * xkw;
                        sumY        += yk * w;
                        sumXY       += yk * xkw;
                    }

                    meanX        = sumX        / sumWeights;
                    meanY        = sumY        / sumWeights;
                    meanXY       = sumXY       / sumWeights;
                    meanXSquared = sumXSquared / sumWeights;

                    if(Math.Abs(meanXSquared - meanX * meanX) < double.Epsilon)
                    {
                        beta = 0;
                    }
                    else
                    {
                        beta = (meanXY - meanX * meanY) / (meanXSquared - meanX * meanX);
                    }

                    alpha = meanY - beta * meanX;

                    res[i]       = beta * x + alpha;
                    residuals[i] = Math.Abs(yval[i] - res[i]);
                }

                // No need to recompute the robustness weights at the last
                // iteration, they won't be needed anymore
                if(iter == robustnessIters)
                {
                    break;
                }

                // Recompute the robustness weights.

                // Find the median residual.
                // An arraycopy and a sort are completely tractable here, 
                // because the preceding loop is a lot more expensive
                Array.Copy(residuals,
                           sortedResiduals,
                           n);

                //System.arraycopy(residuals, 0, sortedResiduals, 0, n);

                Array.Sort(sortedResiduals);

                medianResidual = sortedResiduals[n / 2];

                if(Math.Abs(medianResidual) < double.Epsilon)
                {
                    break;
                }

                for(int i = 0; i < n; ++i)
                {
                    arg = residuals[i] / (6 * medianResidual);

                    robustnessWeights[i] = arg >= 1
                                               ? 0
                                               : Math.Pow(1 - arg * arg,
                                                          2);
                }
            }

            return res;
        }
    }
}
