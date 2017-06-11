package fiuba.algo3.tp1;

import java.util.List;

public class AlgoChat {

	private Usuario usuario;
	private ChatsIndividuales chatsIndividuales;
	private ChatsGrupales chatsGrupales;

	
	public AlgoChat(String nombreUsuario) {
		usuario = new Usuario(nombreUsuario);
		chatsIndividuales = new ChatsIndividuales();
		chatsGrupales = new ChatsGrupales();
	}

	public int cantidadDeChatsIndividuales() {
		return 0;
	}

	public int cantidadDeContactos() {
		return chatsIndividuales.getCantidadDeContactos();
	}
	
	public int cantidadDeChatsGrupales() {
		return 0;
	}

	public int cantidadDeGrupos() {
		return 0;
	}

	public int cantidadDeMensajesEnviados() {
		return 0;
	}

	public String nombreUsuario() {
		return usuario.getNombre();
	}

	public void agregarContacto(String nombreContacto) {
		chatsIndividuales.agregarChat(nombreContacto);
	}

	public boolean existeContacto(String nombreContacto) {
		return chatsIndividuales.existeContacto(nombreContacto);
	}

	public void recibirMensajeDe(String nombreContacto, String mensaje) {
		chatsIndividuales.recibirMensajeDe(nombreContacto, mensaje);
	}

	public int cantidadTotalMensajesRecibidos() {
		return chatsIndividuales.cantidadTotalMensajesRecibidos();
	}

	public int cantidadMensajesDe(String nombreContacto) {
		return chatsIndividuales.cantidadMensajesDe(nombreContacto);
	}

	public void enviarMensajeA(String nombreContacto, String mensaje) {
		chatsIndividuales.enviarMensajeA(nombreContacto, mensaje);
	}

	public int cantidadTotalMensajesEnviados() {
		return chatsIndividuales.cantidadDeMensajesEnviados();
	}

	public int cantidadMensajesEnviadosA(String nombreContacto) {
		return chatsIndividuales.cantidadMensajesEnviadosA(nombreContacto);
	}

	public List<String> obtenerConversacionCon(String nombreContacto) {
		return chatsIndividuales.obtenerConversacionCon(nombreContacto);
	}

	public void crearGrupo(String nombreGrupo) {
		chatsGrupales.agregarChat(nombreGrupo);
		
	}

	public void agregarContactoAGrupo(String nombreGrupo, String nombreContacto) {
		chatsGrupales.agregarContactoAGrupo(nombreGrupo, nombreContacto);
	}

	public Object cantidadMiembrosEnGrupo(String string) {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean existeGrupo(String string) {
		// TODO Auto-generated method stub
		return false;
	}

	public void recibirMensajeDeGrupo(String string, String string2, String string3) {
		// TODO Auto-generated method stub
		
	}

	public void enviarMensajeAGrupo(String string, String string2) {
		// TODO Auto-generated method stub
		
	}

	public Object cantidadMensajesRecibidosDelGrupo(String string) {
		return null;
	}

	public Object cantidadMensajesEnviadosAlGrupo(String string) {
		return null;
	}

	public void borrarMensajesDelContacto(String string) {
		
	}
	
	public void borrarMensajesDelGrupo(String string) {
		
	}





}
