module Main

let time name (f : unit -> 'a) =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let r = f ()
    sw.Stop()
    let ms = float sw.ElapsedTicks / float  System.Diagnostics.Stopwatch.Frequency * 1000.0 
    printfn "%s %f ms." name ms
    ms, r

type NoOpInline =
    struct
        val SUM : float
        new sum = { SUM = sum }        
    end
    static member (+) (lhs : NoOpInline, rhs : NoOpInline) =
        NoOpInline(lhs.SUM + rhs.SUM)

type OpInline =
    struct
        val SUM : float
        new sum = { SUM = sum }        
    end
    static member inline (+) (lhs : OpInline, rhs : OpInline) =
        OpInline(lhs.SUM + rhs.SUM)

let inline fold f a (xs : 'T[]) =
    let mutable r = a
    for i = 0 to xs.Length - 1 do
        r <- f r xs.[i]
    r

// ILSpy "disassembly" for a:
//
// public static Main.NoOpInline a(double[] xs)
// {
//   Main.NoOpInline noOpInline = new Main.NoOpInline(0.0);
//   Main.NoOpInline noOpInline2 = noOpInline;
//   for (int i = 0; i < xs.Length; i++)
//   {
//     Main.NoOpInline noOpInline3 = noOpInline2;
//     double sum = xs[i];
//     Main.NoOpInline noOpInline4 = new Main.NoOpInline(sum);
//     noOpInline2 = new Main.NoOpInline(noOpInline3.SUM@ + noOpInline4.SUM@);
//   }
//   return noOpInline2;
// }
//
let a (xs : float[]) =
    xs |> fold (fun acc x -> acc + NoOpInline x) (NoOpInline 0.0)

// ILSpy "disassembly" for b:
//
// public static Main.OpInline b(double[] xs)
// {
//   Main.OpInline opInline = new Main.OpInline(0.0);
//   Main.OpInline opInline2 = opInline;
//   for (int i = 0; i < xs.Length; i++)
//   {
//     opInline2 = Main.f@28(opInline2, xs[i]);
//   }
//   return opInline2;
// }
//
// [CompilerGenerated]
// internal static Main.OpInline f@28(Main.OpInline acc, double x)
// {
//   Main.OpInline opInline = new Main.OpInline(x);
//   return new Main.OpInline(acc.SUM@ + opInline.SUM@);
// }
//
let b (xs : float[]) =
    xs |> fold (fun acc x -> acc + OpInline x) (OpInline 0.0)

// Output of main (compiled with "Microsoft (R) F# Compiler version 14.0.23413.0"):
//
// Without operator inline 1.811314 ms.
// Without operator inline 1.129896 ms.
// Without operator inline 1.144622 ms.
// With operator inline 6.834265 ms.
// With operator inline 6.592846 ms.
// With operator inline 6.170250 ms.
//
[<EntryPoint>]
let main argv = 
    let n = 1000000
    let xs = Array.init n float
    for i = 0 to 2 do
        time "Without operator inline" (fun () -> a xs) |> ignore
    for i = 0 to 2 do
        time "With operator inline" (fun () -> b xs) |> ignore
    0
