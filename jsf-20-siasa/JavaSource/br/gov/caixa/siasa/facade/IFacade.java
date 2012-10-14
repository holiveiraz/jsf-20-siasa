package br.gov.caixa.siasa.facade;

public interface IFacade {

	public void consultaLancamentoDoacaoEmCheque();

	public void consultaLancamentoEstornoPendente();

	public void inclusaoEstornoCheque();

	public void delecaoEstornoJaComandado();

	public void consultaLancamentoCheque();

	public void consultaApuracaoRealizada();

	public void consultaRemessaJaRealizada();

	public void manutencaoTipoCancal();
}
