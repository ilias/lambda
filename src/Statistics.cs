namespace LambdaCalculus;

// Tracks interpreter statistics and performance metrics.
public class Statistics
{
    public long TimeInCacheLookup { get; set; }
    public long TimeInSubstitution { get; set; }
    public long TimeInEvaluation { get; set; }
    public long TimeInForcing { get; set; }
    public int NormalizeCEKCount { get; set; }
    public int CacheHits { get; set; }
    public int CacheMisses { get; set; }
    public int TotalIterations { get; set; }
    public int Iterations { get; set; }
    public int SubstitutionExprCount { get; set; }
    public int ThunkForceCount { get; set; }
    public int VarCounter { get; set; }
    public int MaxRecursionDepth { get; set; } = 20;
    public int StructEqCalls { get; set; }
    public int StructEqSuccesses { get; set; }
    public void Reset()
    {
        TimeInCacheLookup = 0;
        TimeInSubstitution = 0;
        TimeInEvaluation = 0;
        TimeInForcing = 0;
        NormalizeCEKCount = 0;
        CacheHits = 0;
        CacheMisses = 0;
        TotalIterations = 0;
        Iterations = 0;
        SubstitutionExprCount = 0;
        ThunkForceCount = 0;
        VarCounter = 0;
        StructEqCalls = 0;
        StructEqSuccesses = 0;
    }
}