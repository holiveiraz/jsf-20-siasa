package br.gov.caixa.siasa.facade;

import br.gov.caixa.siasa.model.dto.CobolBook;

public interface IFacade {

	public void consultaLancamentoDoacaoEmCheque(CobolBook cb);

	public void consultaLancamentoEstornoPendente(CobolBook cb);

	public void inclusaoEstornoCheque(CobolBook cb);

	public void delecaoEstornoJaComandado(CobolBook cb);

	public void consultaLancamentoCheque(CobolBook cb);

	public void consultaApuracaoRealizada(CobolBook cb);

	public void consultaRemessaJaRealizada(CobolBook cb);

	public void manutencaoTipoCanal(CobolBook cb);
}
