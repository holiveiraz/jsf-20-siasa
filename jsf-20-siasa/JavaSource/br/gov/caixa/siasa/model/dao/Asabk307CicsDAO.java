package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk307CicsDAO extends MockCicsDAO implements IDao {

	public Asabk307CicsDAO(String string) {
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,47));
		builder.append("003");
		for(long l=0;l<3;l++) {
			builder.append(pic(l+1L,3));
			builder.append(pic("TIPO CANAL #"+(l+1L),60));
		}
		builder.append(pic(FILLER,3267));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
