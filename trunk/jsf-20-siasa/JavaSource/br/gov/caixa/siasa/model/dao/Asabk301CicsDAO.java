package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk301CicsDAO extends MockCicsDAO implements IDao {

	public Asabk301CicsDAO(String string) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		// TODO Auto-generated method stub
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,47));
		builder.append("003");
		for(long l=0;l<3;l++) {
			builder.append(pic(l+1L,12));
			builder.append("12345678901");
			builder.append("12345678901234");
			builder.append("01/01/0001");
			builder.append("02/02/0002");
			builder.append("03/03/0003");
			builder.append("04/04/0004");
			builder.append("05/05/0005");
			builder.append(pic(l+10L,9));
			builder.append(pic(l+1L,4));
			builder.append("06/06/0006");
			builder.append(pic(l+40000L,15));
			builder.append(pic(l+20000L,15));
			builder.append(pic(l+4063,4));
		}
		builder.append(pic(FILLER,3618));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
