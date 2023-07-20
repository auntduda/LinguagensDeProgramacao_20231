module Root.Testes.TestesQ4 where

import Root.Typechecker.Typechecker
import Root.Typechecker.AbsLOO
import Test.Hspec
import Data.List (isInfixOf)

{- Código da Q4

class Q4 {
    void main ( ) {

        const Conta contaNormal = new Conta();
        contaNormal.setNumero ("001");
        contaNormal.setSaldo(100);
        contaNormal.creditar(30);

        const Cliente clienteComContaNormal = new Cliente();
        clienteComContaNormal.setNome("João");
        clienteComContaNormal.setConta(contaNormal);

        const ContaBonificada contaBonificada = new ContaBonificada();
        contaBonificada.setNumero ("002");
        contaBonificada.setSaldo(100);
        contaBonificada.setBonusPorDeposito(3);
        contaBonificada.creditar(50);
        contaBonificada.creditar(20);

        const Cliente clienteComContaBonificada = new Cliente();
        clienteComContaBonificada.setNome("José");
        clienteComContaBonificada.setConta(contaBonificada);

        const Conta cn = clienteComContaBonificada.getConta( );
        
        // >>>> ERRO DE TIPOS: O código abaixo não deve ser aceito pelo nosso TypeChecker, pois
        //      embora a atribuição de 'cn' seja feita para um objeto de 'ContaBonificada',
        //      a declaração de tipo de 'cn' continua sendo 'Conta'.
        cn.resgatarBonus( );

    }
}


class Conta {
    int saldo;
    String numero;

    void setNumero (String n) {
        numero = n;
    }

    String getNumero ( ) {
        return numero;
    }

    int getSaldo ( ) {
        return saldo;
    }

    void setSaldo (int s) {
        saldo = s;
    }

    void creditar (int valor) {
        saldo = saldo + valor;
    }

    void debitar (int valor) {
        saldo = saldo - valor;
    }

    void transferir (Conta destino, int valor) {
        this.debitar (valor);
        destino.creditar(valor);
    }
}


class ContaBonificada extends Conta {
    int bonus;
    int bonusPorDeposito;

    int getBonus ( ) {
        return bonus;
    }

    int getBonusPorDeposito ( ) {
        return bonusPorDeposito;
    }

    void setBonusPorDeposito (int b) {
        bonusPorDeposito = b;
    }

    void resgatarBonus ( ) {
        saldo = saldo + bonus;
        bonus = 0;
    }

    void creditar (int valor) {
        saldo = saldo + valor;
        bonus = bonus + bonusPorDeposito;
    }
}


class Cliente {
    String nome;
    Conta conta;
    
    void setNome(String n) {
        nome = n;
    }

    String getNome( ) {
        return nome;
    }

    void setConta(Conta c) {
        conta = c;
    }

    Conta getConta( ) {
        return conta;
    }
} 

-}

matchesExpectedError :: String -> R TEnvironment -> Bool
matchesExpectedError _ (OK _) = False
matchesExpectedError expectedError (Erro msg) = expectedError `isInfixOf` msg

progQ4 = Prog [ClassD (Ident "Q4") ExtObject [Mth Tvoid (Ident "main") [] [SConstInit (Dec (TClass (Ident "Conta")) (Ident "contaNormal")) (ENew (Ident "Conta")),SExp (EMthCall (Ident "contaNormal") (Ident "setNumero") [EStr "001"]),SExp (EMthCall (Ident "contaNormal") (Ident "setSaldo") [EInt 100]),SExp (EMthCall (Ident "contaNormal") (Ident "creditar") [EInt 30]),SConstInit (Dec (TClass (Ident "Cliente")) (Ident "clienteComContaNormal")) (ENew (Ident "Cliente")),SExp (EMthCall (Ident "clienteComContaNormal") (Ident "setNome") [EStr "Jo\227o"]),SExp (EMthCall (Ident "clienteComContaNormal") (Ident "setConta") [EVar (Ident "contaNormal")]),SConstInit (Dec (TClass (Ident "ContaBonificada")) (Ident "contaBonificada")) (ENew (Ident "ContaBonificada")),SExp (EMthCall (Ident "contaBonificada") (Ident "setNumero") [EStr "002"]),SExp (EMthCall (Ident "contaBonificada") (Ident "setSaldo") [EInt 100]),SExp (EMthCall (Ident "contaBonificada") (Ident "setBonusPorDeposito") [EInt 3]),SExp (EMthCall (Ident "contaBonificada") (Ident "creditar") [EInt 50]),SExp (EMthCall (Ident "contaBonificada") (Ident "creditar") [EInt 20]),SConstInit (Dec (TClass (Ident "Cliente")) (Ident "clienteComContaBonificada")) (ENew (Ident "Cliente")),SExp (EMthCall (Ident "clienteComContaBonificada") (Ident "setNome") [EStr "Jos\233"]),SExp (EMthCall (Ident "clienteComContaBonificada") (Ident "setConta") [EVar (Ident "contaBonificada")]),SConstInit (Dec (TClass (Ident "Conta")) (Ident "cn")) (EMthCall (Ident "clienteComContaBonificada") (Ident "getConta") []),SExp (EMthCall (Ident "cn") (Ident "resgatarBonus") [])]],ClassD (Ident "Conta") ExtObject [Attr (Dec Tint (Ident "saldo")),Attr (Dec TStr (Ident "numero")),Mth Tvoid (Ident "setNumero") [Dec TStr (Ident "n")] [SAss (Ident "numero") (EVar (Ident "n"))],Mth TStr (Ident "getNumero") [] [SReturn (EVar (Ident "numero"))],Mth Tint (Ident "getSaldo") [] [SReturn (EVar (Ident "saldo"))],Mth Tvoid (Ident "setSaldo") [Dec Tint (Ident "s")] [SAss (Ident "saldo") (EVar (Ident "s"))],Mth Tvoid (Ident "creditar") [Dec Tint (Ident "valor")] [SAss (Ident "saldo") (EAdd (EVar (Ident "saldo")) (EVar (Ident "valor")))],Mth Tvoid (Ident "debitar") [Dec Tint (Ident "valor")] [SAss (Ident "saldo") (ESub (EVar (Ident "saldo")) (EVar (Ident "valor")))],Mth Tvoid (Ident "transferir") [Dec (TClass (Ident "Conta")) (Ident "destino"),Dec Tint (Ident "valor")] [SExp (EMthCall (Ident "this") (Ident "debitar") [EVar (Ident "valor")]),SExp (EMthCall (Ident "destino") (Ident "creditar") [EVar (Ident "valor")])]],ClassD (Ident "ContaBonificada") (ExtId (Ident "Conta")) [Attr (Dec Tint (Ident "bonus")),Attr (Dec Tint (Ident "bonusPorDeposito")),Mth Tint (Ident "getBonus") [] [SReturn (EVar (Ident "bonus"))],Mth Tint (Ident "getBonusPorDeposito") [] [SReturn (EVar (Ident "bonusPorDeposito"))],Mth Tvoid (Ident "setBonusPorDeposito") [Dec Tint (Ident "b")] [SAss (Ident "bonusPorDeposito") (EVar (Ident "b"))],Mth Tvoid (Ident "resgatarBonus") [] [SAss (Ident "saldo") (EAdd (EVar (Ident "saldo")) (EVar (Ident "bonus"))),SAss (Ident "bonus") (EInt 0)],Mth Tvoid (Ident "creditar") [Dec Tint (Ident "valor")] [SAss (Ident "saldo") (EAdd (EVar (Ident "saldo")) (EVar (Ident "valor"))),SAss (Ident "bonus") (EAdd (EVar (Ident "bonus")) (EVar (Ident "bonusPorDeposito")))]],ClassD (Ident "Cliente") ExtObject [Attr (Dec TStr (Ident "nome")),Attr (Dec (TClass (Ident "Conta")) (Ident "conta")),Mth Tvoid (Ident "setNome") [Dec TStr (Ident "n")] [SAss (Ident "nome") (EVar (Ident "n"))],Mth TStr (Ident "getNome") [] [SReturn (EVar (Ident "nome"))],Mth Tvoid (Ident "setConta") [Dec (TClass (Ident "Conta")) (Ident "c")] [SAss (Ident "conta") (EVar (Ident "c"))],Mth (TClass (Ident "Conta")) (Ident "getConta") [] [SReturn (EVar (Ident "conta"))]]]
typeCheckP4 = typeCheckProgram progQ4
testP4 = any (matchesExpectedError "QUESTAO4") typeCheckP4

--------------------- VERIFICAÇÃO DOS TESTES ---------------------
main = hspec $ do
  describe "Suite de Testes do Trabalho 04, Exercício 4" $ do
    it "O teste do programa deve retornar True" $ do
      testP4 `shouldBe` True