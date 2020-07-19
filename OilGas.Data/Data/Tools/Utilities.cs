using System.Linq;
using System.Runtime.CompilerServices;

namespace OilGas.Data
{
    public static class Utilities
    {
#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static float[] CumulativeSum(float[] rate)
        {
            float[] output = new float[rate.Length];

            output[0] = rate[0];

            for(int i = 1; i < rate.Length; i++)
            {
                output[i] = output[i - 1] + rate[i];
            }

            return output;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static double _scan(double lhs,
                                   double rhs)
        {
            lhs += rhs;
            return lhs;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static double[] CumulativeSum(double[] rate, int startIndex = 0, bool parallel = false)
        {
            double[] output = new double[rate.Length];

            if(parallel)
            {
                output = System.Threading.Algorithms.ParallelAlgorithms.Scan(rate.Skip(startIndex),
                                                                             _scan);
            }
            else
            {
                output[startIndex] = rate[startIndex];

                for(int i = startIndex + 1; i < rate.Length; i++)
                {
                    output[i] = output[i - 1] + rate[i];
                }
            }

            return output;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static float[] CumulativeAvgSum(float[] rate,
                                               float[] days)
        {
            float[] output = new float[rate.Length];

            output[0] = rate[0] * days[0];

            for(int i = 1; i < rate.Length; i++)
            {
                output[i] = output[i - 1] + rate[i] * days[i];
            }

            return output;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static double[] CumulativeAvgSum(double[] rate,
                                                double[] days)
        {
            double[] output = new double[rate.Length];

            output[0] = rate[0] * days[0];

            for(int i = 1; i < rate.Length; i++)
            {
                output[i] = output[i - 1] + rate[i] * days[i];
            }

            return output;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static float[] CumulativeSum(float[] rate,
                                            float[] rateOverride,
                                            int     untilIndex)
        {
            float[] output = new float[rate.Length];

            output[0] = rateOverride[0];

            for(int i = 1; i < untilIndex; i++)
            {
                output[i] = output[i - 1] + rateOverride[i];
            }

            for(int i = untilIndex; i < rate.Length; i++)
            {
                output[i] = output[i - 1] + rate[i];
            }

            return output;
        }

#if NETCOREAPP
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#else
        [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
#endif
        public static double[] CumulativeSum(double[] rate,
                                             double[] rateOverride,
                                             int      untilIndex)
        {
            double[] output = new double[rate.Length];

            output[0] = rateOverride[0];

            for(int i = 1; i < untilIndex; i++)
            {
                output[i] = output[i - 1] + rateOverride[i];
            }

            for(int i = untilIndex; i < rate.Length; i++)
            {
                output[i] = output[i - 1] + rate[i];
            }

            return output;
        }
    }
}
