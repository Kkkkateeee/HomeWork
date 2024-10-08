open FsCheck
open NUnit.Framework

// Функция сложения
let add a b = a + b

// Тестирование коммутативности
[<Test>]
let Addition is commutative () =
    Prop.forAll (Arb.fromGen (Gen.int32), Arb.fromGen (Gen.int32)) (fun a b ->
        add a b = add b a)
    |> Prop.quickCheckThrowOnFailure
