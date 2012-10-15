package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk304CicsDAO extends MockCicsDAO implements IDao {

	public Asabk304CicsDAO(String string) {
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,42));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
