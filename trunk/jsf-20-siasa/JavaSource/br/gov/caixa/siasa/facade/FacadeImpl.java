package br.gov.caixa.siasa.facade;

import br.gov.caixa.siasa.model.dao.Asabk304CicsDAO;
import br.gov.caixa.siasa.model.dao.Asabk307CicsDAO;
import br.gov.caixa.siasa.model.dao.Asabk308CicsDAO;
import br.gov.caixa.siasa.model.dao.CicsDAO;
import br.gov.caixa.siasa.model.dao.IDao;
import br.gov.caixa.siasa.model.dto.CobolBook;

public class FacadeImpl implements IFacade {

	private IDao dao;
	
	@Override
	public void consultaLancamentoDoacaoEmCheque(final CobolBook cb) {
		dao = new CicsDAO("ASAPO301");
		dao.execute(cb);
	}

	@Override
	public void consultaLancamentoEstornoPendente(final CobolBook cb) {
		dao = new CicsDAO("ASAPO302");
		dao.execute(cb);
	}

	@Override
	public void inclusaoEstornoCheque(final CobolBook cb) {
		dao = new CicsDAO("ASAPO303");
		dao.execute(cb);
	}

	@Override
	public void delecaoEstornoJaComandado(final CobolBook cb) {
//		dao = new CicsDAO("ASAPO304");
		dao = new Asabk304CicsDAO("ASAPO304");
		dao.execute(cb);
	}

	@Override
	public void consultaLancamentoCheque(final CobolBook cb) {
		dao = new CicsDAO("ASAPO305");
		dao.execute(cb);
	}

	@Override
	public void consultaApuracaoRealizada(final CobolBook cb) {
		dao = new CicsDAO("ASAPO306");
		dao.execute(cb);
	}

	@Override
	public void consultaRemessaJaRealizada(final CobolBook cb) {
//		dao = new CicsDAO("ASAPO307");
		dao = new Asabk307CicsDAO("ASAPO307");
		dao.execute(cb);
	}

	@Override
	public void manutencaoTipoCancal(final CobolBook cb) {
//		dao = new CicsDAO("ASAPO308");
		dao = new Asabk308CicsDAO("ASAPO308");
		dao.execute(cb);
	}

}
