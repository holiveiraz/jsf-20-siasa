package br.gov.caixa.siasa.facade;

import br.gov.caixa.siasa.model.dao.CicsDaoFactory;
import br.gov.caixa.siasa.model.dao.IDao;
import br.gov.caixa.siasa.model.dto.CobolBook;

public class FacadeImpl implements IFacade {

	private IDao dao = null;
	
	@Override
	public void consultaLancamentoDoacaoEmCheque(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO301");// ASAPO301
		dao.execute(cb);
	}

	@Override
	public void consultaLancamentoEstornoPendente(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO302");// ASAPO302
		dao.execute(cb);
	}

	@Override
	public void inclusaoEstornoCheque(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO303");// ASAPO303
		dao.execute(cb);
	}

	@Override
	public void delecaoEstornoJaComandado(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO304");// ASAPO304
		dao.execute(cb);
	}

	@Override
	public void consultaLancamentoCheque(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO305");// ASAPO305
		dao.execute(cb);
	}

	@Override
	public void consultaApuracaoRealizada(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO306");// ASAPO306
		dao.execute(cb);
	}

	@Override
	public void consultaRemessaJaRealizada(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO307");// ASAPO307
		dao.execute(cb);
	}

	@Override
	public void manutencaoTipoCanal(final CobolBook cb) {
		dao = CicsDaoFactory.getDao("MOCKPO308");// ASAPO308
		dao.execute(cb);
	}

}
